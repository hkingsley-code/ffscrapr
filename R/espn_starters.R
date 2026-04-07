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
  matchup_periods <- checkmax$matchup_periods

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

  raw_starters <- purrr::map_dfr(run_weeks, ~.espn_week_starter(.x, conn, matchup_periods))

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
  url_query <- glue::glue(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId=0&view=mSettings"
  )

  settings <- espn_getendpoint_raw(conn, url_query)

  current_scoring_period <- settings %>%
    purrr::pluck("content", "status", "latestScoringPeriod")

  final_scoring_period <- settings %>%
    purrr::pluck("content", "status", "finalScoringPeriod")

  # matchupPeriods: named list, keys are matchupPeriodId (as character),
  # values are integer vectors of scoringPeriodIds in that matchup week.
  # e.g. list("1" = 1:7, "2" = 8:14, ...)
  matchup_periods <- settings %>%
    purrr::pluck("content", "settings", "scheduleSettings", "matchupPeriods")

  max_scoring_period <- min(current_scoring_period, final_scoring_period, na.rm = TRUE)

  if (!is.null(matchup_periods) && length(matchup_periods) > 0) {
    last_scoring_per_matchup <- purrr::map_int(matchup_periods, ~max(as.integer(.x)))
    completed_weeks <- which(last_scoring_per_matchup <= max_scoring_period)
    max_week <- if (length(completed_weeks) > 0) max(completed_weeks) else 0L
  } else {
    matchup_period_length <- settings %>%
      purrr::pluck("content", "settings", "scheduleSettings", "matchupPeriodLength", .default = 1L)
    max_week <- floor(max_scoring_period / matchup_period_length)
  }

  list(max_week = max_week, matchup_periods = matchup_periods)
}

.espn_week_starter <- function(week, conn, matchup_periods = NULL) {
  if (!is.null(matchup_periods) && !is.null(matchup_periods[[as.character(week)]])) {
    scoring_period_id <- max(as.integer(matchup_periods[[as.character(week)]]))
  } else {
    scoring_period_id <- week
  }

  # mBoxscore view adds rosterForMatchupPeriod (weekly cumulative stats) and
  # rosterForCurrentScoringPeriod to each schedule home/away entry.
  # rosterForMatchupPeriod has the correct per-player totals for the full week.
  url_query <- glue::glue(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId={scoring_period_id}&view=mMatchupScore&view=mBoxscore"
  )

  schedule <- espn_getendpoint_raw(conn, url_query) %>%
    purrr::pluck("content", "schedule")

  if (is.null(schedule) || length(schedule) == 0) return(tibble::tibble())

  tbl <- tibble::tibble(x = schedule) %>%
    tidyr::hoist("x", "week" = "matchupPeriodId", "home", "away") %>%
    dplyr::filter(.data$week == .env$week)
  message("[D] rows for week=", week, ": ", nrow(tbl))

  pivoted <- tidyr::pivot_longer(tbl, c(.data$home, .data$away),
                                  names_to = NULL, values_to = "team")
  list_teams <- dplyr::filter(pivoted, purrr::map_lgl(.data$team, is.list))
  message("[D] rows after is.list(team): ", nrow(list_teams))

  if (nrow(list_teams) > 0) {
    t1 <- list_teams$team[[1]]
    message("[D] team keys: ", paste(names(t1), collapse=", "))
    rfmp <- t1$rosterForMatchupPeriod
    message("[D] rosterForMatchupPeriod type: ", class(rfmp)[1],
            " is.list: ", is.list(rfmp))
    if (is.list(rfmp)) message("[D] rosterForMatchupPeriod keys: ",
                               paste(names(rfmp), collapse=", "))
  }

  raw <- list_teams %>%
    tidyr::hoist("team",
                 "franchise_id"    = "teamId",
                 "franchise_score" = "totalPoints",
                 "starting_lineup" = "rosterForMatchupPeriod") %>%
    dplyr::select(-"team", -"x") %>%
    dplyr::filter(purrr::map_lgl(.data$starting_lineup, is.list))
  message("[D] rows after is.list(starting_lineup): ", nrow(raw))

  if (nrow(raw) == 0) return(tibble::tibble())

  week_scores <- raw %>%
    tidyr::hoist("starting_lineup", "entries") %>%
    tidyr::unnest_longer("entries") %>%
    dplyr::filter(purrr::map_lgl(.data$entries, is.list))

  if (nrow(week_scores) == 0) return(tibble::tibble())

  week_scores <- week_scores %>%
    tidyr::hoist("entries",
                 "player_id"   = "playerId",
                 "lineup_id"   = "lineupSlotId",
                 "player_data" = "playerPoolEntry") %>%
    dplyr::filter(purrr::map_lgl(.data$player_data, is.list))

  if (nrow(week_scores) == 0) return(tibble::tibble())

  week_scores <- week_scores %>%
    tidyr::hoist("player_data", "player_score" = "appliedStatTotal", "player") %>%
    dplyr::select(-"player_data") %>%
    dplyr::filter(purrr::map_lgl(.data$player, is.list))

  if (nrow(week_scores) == 0) return(tibble::tibble())

  week_scores %>%
    tidyr::hoist("player",
                 "eligible_lineup_slots" = "eligibleSlots",
                 "player_name"           = "fullName",
                 "pos"                   = "defaultPositionId",
                 "team"                  = "proTeamId") %>%
    dplyr::select(-"player") %>%
    dplyr::mutate(week = .env$week)
}
