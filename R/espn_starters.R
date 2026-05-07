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
  max_week     <- checkmax$max_week
  matchup_days <- checkmax$matchup_days

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

  raw_starters <- purrr::map_dfr(run_weeks, ~.espn_week_starter(.x, conn, matchup_days))

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

  # Build matchupPeriod → all daily scoring period IDs from schedule entries.
  # Each schedule entry's home$pointsByScoringPeriod has daily period IDs as names.
  schedule <- purrr::pluck(content, "schedule")

  # Collect all unique daily period IDs per matchup week
  matchup_days_raw <- purrr::map_dfr(schedule, function(entry) {
    pbs <- entry$home$pointsByScoringPeriod
    if (!is.null(pbs) && length(pbs) > 0) {
      days <- suppressWarnings(as.integer(names(pbs)))
      days <- days[!is.na(days)]
      if (length(days) > 0) {
        return(tibble::tibble(
          matchup = as.integer(entry$matchupPeriodId),
          day     = days
        ))
      }
    }
    tibble::tibble(matchup = integer(0L), day = integer(0L))
  }) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$matchup) %>%
    dplyr::summarise(
      days      = list(sort(unique(.data$day))),
      first_day = min(.data$day),
      last_day  = max(.data$day),
      .groups   = "drop"
    )

  # Max available week: last matchup whose first day has already occurred.
  started  <- matchup_days_raw$matchup[matchup_days_raw$first_day <= max_scoring_period]
  max_week <- if (length(started) > 0) max(started) else 0L

  # Named list: matchupPeriodId → integer vector of daily scoring period IDs
  # Cap each day at max_scoring_period so we don't query future days.
  matchup_days <- purrr::map(matchup_days_raw$days, function(d) {
    d[d <= max_scoring_period]
  })
  names(matchup_days) <- as.character(matchup_days_raw$matchup)

  list(max_week = max_week, matchup_days = matchup_days)
}

.espn_day_roster <- function(day, week, conn) {
  url_query <- glue::glue(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId={day}&view=mMatchupScore&view=mBoxscore&view=mRoster"
  )

  schedule <- espn_getendpoint_raw(conn, url_query) %>%
    purrr::pluck("content", "schedule")

  if (is.null(schedule) || length(schedule) == 0) return(tibble::tibble())

  # Filter schedule to just the target week, pivot home/away into rows
  week_teams <- tibble::tibble(x = schedule) %>%
    tidyr::hoist("x", "week" = "matchupPeriodId", "home", "away") %>%
    dplyr::filter(.data$week == .env$week) %>%
    tidyr::pivot_longer(c(.data$home, .data$away), names_to = NULL, values_to = "team") %>%
    dplyr::filter(purrr::map_lgl(.data$team, is.list)) %>%
    tidyr::hoist("team",
                 "franchise_id"    = "teamId",
                 "franchise_score" = "totalPoints",
                 "slot_roster"     = "rosterForCurrentScoringPeriod") %>%
    dplyr::select(-"team", -"x") %>%
    dplyr::filter(purrr::map_lgl(.data$slot_roster, is.list))

  if (nrow(week_teams) == 0) return(tibble::tibble())

  # Extract per-player slot and daily appliedStatTotal from rosterForCurrentScoringPeriod.
  # This roster field correctly reflects the lineup slot for THIS day, and
  # appliedStatTotal here is the stats accumulated during this scoring period.
  week_teams %>%
    dplyr::select("franchise_id", "franchise_score", "slot_roster") %>%
    tidyr::hoist("slot_roster", "entries" = "entries") %>%
    dplyr::filter(purrr::map_lgl(.data$entries, is.list)) %>%
    tidyr::unnest_longer("entries") %>%
    dplyr::filter(purrr::map_lgl(.data$entries, is.list)) %>%
    tidyr::hoist("entries",
                 "player_id"   = "playerId",
                 "lineup_id"   = "lineupSlotId",
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
    dplyr::select(-"player") %>%
    dplyr::mutate(scoring_day = .env$day)
}

.espn_day_raw_stats <- function(day, week, conn) {
  url_query <- glue::glue(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId={day}&view=mMatchupScore&view=mBoxscore&view=mRoster"
  )

  schedule <- espn_getendpoint_raw(conn, url_query) %>%
    purrr::pluck("content", "schedule")

  if (is.null(schedule) || length(schedule) == 0) return(tibble::tibble())

  week_teams <- tibble::tibble(x = schedule) %>%
    tidyr::hoist("x", "week" = "matchupPeriodId", "home", "away") %>%
    dplyr::filter(.data$week == .env$week) %>%
    tidyr::pivot_longer(c(.data$home, .data$away), names_to = NULL, values_to = "team") %>%
    dplyr::filter(purrr::map_lgl(.data$team, is.list)) %>%
    tidyr::hoist("team",
                 "franchise_id" = "teamId",
                 "slot_roster"  = "rosterForCurrentScoringPeriod") %>%
    dplyr::select(-"team", -"x") %>%
    dplyr::filter(purrr::map_lgl(.data$slot_roster, is.list))

  if (nrow(week_teams) == 0) return(tibble::tibble())

  stat_map <- .espn_stat_map()

  base <- week_teams %>%
    dplyr::select("franchise_id", "slot_roster") %>%
    tidyr::hoist("slot_roster", "entries" = "entries") %>%
    dplyr::filter(purrr::map_lgl(.data$entries, is.list)) %>%
    tidyr::unnest_longer("entries") %>%
    dplyr::filter(purrr::map_lgl(.data$entries, is.list)) %>%
    tidyr::hoist("entries",
                 "player_id"   = "playerId",
                 "lineup_id"   = "lineupSlotId",
                 "player_data" = "playerPoolEntry") %>%
    dplyr::filter(purrr::map_lgl(.data$player_data, is.list)) %>%
    tidyr::hoist("player_data", "raw_stats" = "stats") %>%
    dplyr::select("franchise_id", "player_id", "lineup_id", "raw_stats") %>%
    dplyr::mutate(
      stat_tbl = purrr::map(.data$raw_stats, function(s) {
        if (is.null(s) || length(s) == 0) return(tibble::tibble())
        # statSplitTypeId 5 = specific scoring period; statSourceId 0 = actual (not projected)
        matching <- purrr::keep(s, function(x) {
          isTRUE(x[["statSplitTypeId"]] == 5) && isTRUE(x[["statSourceId"]] == 0)
        })
        if (length(matching) == 0) return(tibble::tibble())
        stat_dict <- matching[[1]][["stats"]]
        if (is.null(stat_dict) || length(stat_dict) == 0) return(tibble::tibble())
        nms   <- stat_map[names(stat_dict)]
        valid <- !is.na(nms)
        if (!any(valid)) return(tibble::tibble())
        tibble::tibble(stat = nms[valid], value = as.numeric(unlist(stat_dict)[valid]))
      }),
      raw_stats = NULL
    )

  if (all(purrr::map_lgl(base$stat_tbl, ~ nrow(.x) == 0))) return(tibble::tibble())

  base %>%
    tidyr::unnest("stat_tbl") %>%
    tidyr::pivot_wider(
      names_from  = "stat",
      values_from = "value",
      values_fill = 0
    ) %>%
    dplyr::mutate(scoring_day = .env$day)
}

.espn_week_raw_stats <- function(week, conn, matchup_days = NULL) {
  week_days <- if (!is.null(matchup_days[[as.character(week)]])) {
    matchup_days[[as.character(week)]]
  } else {
    week
  }

  daily <- purrr::map_dfr(week_days, .espn_day_raw_stats, week = week, conn = conn)

  if (nrow(daily) == 0) return(tibble::tibble())

  key_cols  <- c("franchise_id", "player_id", "lineup_id", "scoring_day")
  stat_cols <- names(daily)[!names(daily) %in% key_cols]
  stat_cols <- stat_cols[vapply(daily[stat_cols], is.numeric, logical(1L))]

  daily %>%
    dplyr::group_by(.data$franchise_id, .data$player_id, .data$lineup_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(stat_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(week = .env$week)
}

#' Get the latest completed scoring week for an ESPN league
#'
#' Returns the highest matchup week that has already started, based on ESPN's
#' current scoring period. Useful for determining which week to pass to
#' \code{\link{get_weekly_points}} without fetching all weeks first.
#'
#' @param conn A connection object created by \code{\link{espn_connect}}.
#'
#' @return An integer giving the latest available matchup week number.
#'
#' @examples
#' \donttest{
#' try({
#'   conn <- espn_connect(season = 2026, league_id = 85601)
#'   espn_current_week(conn)
#' })
#' }
#'
#' @export
espn_current_week <- function(conn) {
  .espn_week_checkmax(conn)$max_week
}

.espn_week_starter <- function(week, conn, matchup_days = NULL) {
  week_days <- if (!is.null(matchup_days[[as.character(week)]])) {
    matchup_days[[as.character(week)]]
  } else {
    week
  }

  # Query each day in the matchup week individually.
  # rosterForCurrentScoringPeriod gives us the correct slot for that day
  # and the points scored during that day.
  daily_rosters <- purrr::map_dfr(week_days, .espn_day_roster, week = week, conn = conn)

  if (nrow(daily_rosters) == 0) return(tibble::tibble())

  # franchise_score should come from the LAST queried day (most up-to-date total)
  franchise_scores <- daily_rosters %>%
    dplyr::filter(.data$scoring_day == max(.data$scoring_day)) %>%
    dplyr::distinct(.data$franchise_id, .data$franchise_score)

  # Sum each player's points within each slot they occupied across all days.
  # A player who pitches as SP on day 1 and sits on bench day 2-7 will have
  # their SP day summed, giving correct slot attribution.
  daily_rosters %>%
    dplyr::group_by(
      .data$franchise_id,
      .data$player_id,
      .data$lineup_id,
      .data$player_name,
      .data$pos,
      .data$team,
      .data$eligible_lineup_slots
    ) %>%
    dplyr::summarise(player_score = sum(.data$player_score, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(franchise_scores, by = "franchise_id") %>%
    dplyr::mutate(week = .env$week)
}
