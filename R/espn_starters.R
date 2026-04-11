#### ESPN ff_starters ####

#' Get total hitter points scored by each team in a given week
#'
#' Filters started players to hitting lineup slots only (excludes SP, RP, P,
#' BE, and IL) and returns the sum of points scored per team per week.
#'
#' @param conn the connection object created by `ff_connect()`
#' @param weeks which weeks to retrieve, a number or numeric vector
#' @param ... other arguments passed to `ff_starters()`
#'
#' @return a tibble with columns: week, franchise_id, franchise_name, hitter_points
#'
#' @examples
#' \donttest{
#' try({
#'   conn <- espn_connect(season = 2020, league_id = 1178049)
#'   espn_hitter_points(conn, weeks = 1)
#' })
#' }
#'
#' @export
espn_hitter_points <- function(conn, weeks = 1:26, ...) {
  .hitter_slots <- c(
    "C", "1B", "2B", "3B", "SS", "OF",
    "2B/SS", "1B/3B", "LF", "CF", "RF", "DH", "UTIL", "IF"
  )

  ff_starters(conn, weeks = weeks, ...) %>%
    dplyr::filter(.data$lineup_slot %in% .hitter_slots) %>%
    dplyr::group_by(.data$week, .data$franchise_id, .data$franchise_name) %>%
    dplyr::summarise(hitter_points = sum(.data$player_score, na.rm = TRUE), .groups = "drop")
}

#' Get total pitcher points scored by each team in a given week
#'
#' Filters started players to pitching lineup slots only (SP, RP, P) and
#' returns the sum of points scored per team per week.
#'
#' @param conn the connection object created by `ff_connect()`
#' @param weeks which weeks to retrieve, a number or numeric vector
#' @param ... other arguments passed to `ff_starters()`
#'
#' @return a tibble with columns: week, franchise_id, franchise_name, pitcher_points
#'
#' @examples
#' \donttest{
#' try({
#'   conn <- espn_connect(season = 2020, league_id = 1178049)
#'   espn_pitcher_points(conn, weeks = 1)
#' })
#' }
#'
#' @export
espn_pitcher_points <- function(conn, weeks = 1:26, ...) {
  .pitcher_slots <- c("SP", "RP", "P")

  ff_starters(conn, weeks = weeks, ...) %>%
    dplyr::filter(.data$lineup_slot %in% .pitcher_slots) %>%
    dplyr::group_by(.data$week, .data$franchise_id, .data$franchise_name) %>%
    dplyr::summarise(pitcher_points = sum(.data$player_score, na.rm = TRUE), .groups = "drop")
}


#' Get starters and bench
#'
#' @param conn the connection object created by `ff_connect()`
#' @param weeks which weeks to calculate, a number or numeric vector
#' @param ... other arguments (currently unused)
#'
#' @describeIn ff_starters ESPN: returns who was started as well as what they scored.
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   conn <- espn_connect(season = 2020, league_id = 1178049)
#'   ff_starters(conn, weeks = 1:3)
#' }) # end try
#' }
#'
#' @export
ff_starters.espn_conn <- function(conn, weeks = 1:26, ...) {
  if (conn$season < 2018) stop("Starting lineups not available before 2018")

  checkmate::assert_numeric(weeks)

  checkmax <- .espn_week_checkmax(conn)
  max_week        <- checkmax$max_week
  matchup_last_day <- checkmax$matchup_last_day

  run_weeks <- weeks[weeks <= max_week]

  if (length(run_weeks) == 0) {
    warning(
      glue::glue(
        "ESPN league_id {conn$league_id} does not have lineups for ",
        "{conn$season} weeks {paste(min(weeks),max(weeks), sep = '-')}."
      ),
      call. = FALSE
    )

    return(NULL)
  }

  raw_starters <- purrr::map_dfr(run_weeks, ~.espn_week_starter(.x, conn, matchup_last_day))

  if (nrow(raw_starters) == 0) return(NULL)

  starters <- raw_starters %>%
    dplyr::mutate(
      lineup_slot = .espn_lineupslot_map()[as.character(.data$lineup_id)] %>% unname(),
      pos = .espn_pos_map()[as.character(.data$pos)] %>% unname(),
      team = .espn_team_map()[as.character(.data$team)] %>% unname()
    ) %>%
    dplyr::arrange(.data$week, .data$franchise_id, .data$lineup_id) %>%
    dplyr::left_join(
      ff_franchises(conn) %>% dplyr::select("franchise_id", "franchise_name"),
      by = "franchise_id"
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "week",
      "franchise_id",
      "franchise_name",
      "franchise_score",
      "lineup_slot",
      "player_score",
      "projected_score",
      "player_id",
      "player_name",
      "pos",
      "team",
      "eligible_lineup_slots"
    )))

  return(starters)
}

.espn_week_checkmax <- function(conn) {
  # Include mMatchupScore so we can read pointsByScoringPeriod from the schedule.
  # pointsByScoringPeriod keys are the actual DAILY scoring period IDs in each
  # matchup week — the only reliable way to map matchup week → daily period range.
  url_query <- glue::glue(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId=1&view=mSettings&view=mMatchupScore"
  )

  content <- espn_getendpoint_raw(conn, url_query) %>%
    purrr::pluck("content")

  current_scoring_period <- purrr::pluck(content, "status", "latestScoringPeriod")
  final_scoring_period   <- purrr::pluck(content, "status", "finalScoringPeriod")
  max_scoring_period     <- min(current_scoring_period, final_scoring_period, na.rm = TRUE)

  # Build matchupPeriod → last daily scoring period from schedule entries.
  # Each schedule entry's home$pointsByScoringPeriod has daily period IDs as names.
  schedule <- purrr::pluck(content, "schedule")

  matchup_last_day <- purrr::map_dfr(schedule, function(entry) {
    pbs <- entry$home$pointsByScoringPeriod
    if (!is.null(pbs) && length(pbs) > 0) {
      days <- suppressWarnings(as.integer(names(pbs)))
      days <- days[!is.na(days)]
      if (length(days) > 0) {
        return(tibble::tibble(matchup = as.integer(entry$matchupPeriodId),
                              last_day = max(days),
                              first_day = min(days)))
      }
    }
    tibble::tibble(matchup = integer(0L), last_day = integer(0L), first_day = integer(0L))
  }) %>%
    dplyr::group_by(.data$matchup) %>%
    dplyr::summarise(last_day  = max(.data$last_day),
                     first_day = min(.data$first_day),
                     .groups = "drop")

  # Max available week: last matchup whose first day has already occurred.
  started <- matchup_last_day$matchup[matchup_last_day$first_day <= max_scoring_period]
  max_week <- if (length(started) > 0) max(started) else 0L

  # Named vector: matchupPeriodId → last daily scoring period (capped at max_scoring_period)
  last_day_vec <- pmin(matchup_last_day$last_day, max_scoring_period)
  names(last_day_vec) <- as.character(matchup_last_day$matchup)

  list(max_week = max_week, matchup_last_day = last_day_vec)
}

.espn_week_starter <- function(week, conn, matchup_last_day = NULL) {
  scoring_period_id <- if (!is.null(matchup_last_day[[as.character(week)]])) {
    matchup_last_day[[as.character(week)]]
  } else {
    week
  }

  url_query <- glue::glue(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId={scoring_period_id}&view=mMatchupScore&view=mBoxscore&view=mRoster"
  )

  schedule <- espn_getendpoint_raw(conn, url_query) %>%
    purrr::pluck("content", "schedule")

  if (is.null(schedule) || length(schedule) == 0) return(tibble::tibble())

  # Filter schedule to just the target week and pivot home/away into rows
  week_teams <- tibble::tibble(x = schedule) %>%
    tidyr::hoist("x", "week" = "matchupPeriodId", "home", "away") %>%
    dplyr::filter(.data$week == .env$week) %>%
    tidyr::pivot_longer(c(.data$home, .data$away), names_to = NULL, values_to = "team") %>%
    dplyr::filter(purrr::map_lgl(.data$team, is.list)) %>%
    tidyr::hoist("team",
                 "franchise_id"    = "teamId",
                 "franchise_score" = "totalPoints",
                 "slot_roster"     = "rosterForCurrentScoringPeriod",
                 "score_roster"    = "rosterForMatchupPeriod") %>%
    dplyr::select(-"team", -"x") %>%
    dplyr::filter(purrr::map_lgl(.data$score_roster, is.list))

  if (nrow(week_teams) == 0) return(tibble::tibble())

  # --- Weekly scores from rosterForMatchupPeriod (correct cumulative totals) ---
  scores <- week_teams %>%
    dplyr::select("franchise_id", "franchise_score", "score_roster") %>%
    tidyr::hoist("score_roster", "score_entries" = "entries") %>%
    dplyr::filter(purrr::map_lgl(.data$score_entries, is.list)) %>%
    tidyr::unnest_longer("score_entries") %>%
    dplyr::filter(purrr::map_lgl(.data$score_entries, is.list)) %>%
    tidyr::hoist("score_entries",
                 "player_id"   = "playerId",
                 "player_data" = "playerPoolEntry") %>%
    dplyr::filter(purrr::map_lgl(.data$player_data, is.list)) %>%
    tidyr::hoist("player_data", "player_score" = "appliedStatTotal", "player") %>%
    dplyr::select(-"player_data") %>%
    dplyr::filter(purrr::map_lgl(.data$player, is.list)) %>%
    tidyr::hoist("player",
                 "eligible_lineup_slots" = "eligibleSlots",
                 "player_name"           = "fullName",
                 "pos"                   = "defaultPositionId",
                 "team"                  = "proTeamId") %>%
    dplyr::select(-"player")

  if (nrow(scores) == 0) return(tibble::tibble())

  # --- Lineup slots from rosterForCurrentScoringPeriod (has real lineupSlotId) ---
  slots <- week_teams %>%
    dplyr::select("franchise_id", "slot_roster") %>%
    dplyr::filter(purrr::map_lgl(.data$slot_roster, is.list)) %>%
    tidyr::hoist("slot_roster", "slot_entries" = "entries") %>%
    dplyr::filter(purrr::map_lgl(.data$slot_entries, is.list)) %>%
    tidyr::unnest_longer("slot_entries") %>%
    dplyr::filter(purrr::map_lgl(.data$slot_entries, is.list)) %>%
    tidyr::hoist("slot_entries", "player_id" = "playerId", "lineup_id" = "lineupSlotId") %>%
    dplyr::select("franchise_id", "player_id", "lineup_id")

  # Join lineup slots onto scores
  scores %>%
    dplyr::left_join(slots, by = c("franchise_id", "player_id")) %>%
    dplyr::mutate(week = .env$week)
}
