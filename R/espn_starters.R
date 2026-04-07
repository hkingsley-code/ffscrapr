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

  message("[checkmax] current_scoring_period=", current_scoring_period,
          " final_scoring_period=", final_scoring_period,
          " matchup_periods length=", length(matchup_periods))

  if (!is.null(matchup_periods) && length(matchup_periods) > 0) {
    last_scoring_per_matchup <- purrr::map_int(matchup_periods, ~max(as.integer(.x)))
    message("[checkmax] last scoring period per matchup week: ",
            paste(names(last_scoring_per_matchup), last_scoring_per_matchup, sep="=", collapse=", "))
    completed_weeks <- which(last_scoring_per_matchup <= max_scoring_period)
    max_week <- if (length(completed_weeks) > 0) max(completed_weeks) else 0L
  } else {
    matchup_period_length <- settings %>%
      purrr::pluck("content", "settings", "scheduleSettings", "matchupPeriodLength", .default = 1L)
    message("[checkmax] matchupPeriods NULL, falling back to matchupPeriodLength=", matchup_period_length)
    max_week <- floor(max_scoring_period / matchup_period_length)
  }

  message("[checkmax] max_week=", max_week)
  list(max_week = max_week, matchup_periods = matchup_periods)
}

.espn_week_starter <- function(week, conn, matchup_periods = NULL) {
  if (!is.null(matchup_periods) && !is.null(matchup_periods[[as.character(week)]])) {
    scoring_period_id <- max(as.integer(matchup_periods[[as.character(week)]]))
  } else {
    scoring_period_id <- week
  }
  message("[week_starter] week=", week, " scoring_period_id=", scoring_period_id)
  url_query <- glue::glue(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
    "{conn$season}/segments/0/leagues/{conn$league_id}",
    "?scoringPeriodId={scoring_period_id}&view=mMatchupScore&view=mBoxscore&view=mSettings&view=mRosterSettings"
  )

  api_result <- espn_getendpoint_raw(conn, url_query)
  schedule <- purrr::pluck(api_result, "content", "schedule")
  message("[week_starter] schedule length=", length(schedule))

  raw_tbl <- schedule %>%
    tibble::tibble() %>%
    purrr::set_names("x") %>%
    tidyr::hoist(1, "week" = "matchupPeriodId", "home", "away")

  message("[week_starter] matchupPeriodIds in response: ", paste(unique(raw_tbl$week), collapse=", "))

  week_rows <- dplyr::filter(raw_tbl, .data$week == .env$week)
  message("[week_starter] rows for week=", week, ": ", nrow(week_rows))

  if (nrow(week_rows) > 0 && !is.null(week_rows$home[[1]])) {
    message("[week_starter] first home team fields: ",
            paste(names(week_rows$home[[1]]), collapse=", "))
  }

  pivoted <- tidyr::pivot_longer(week_rows, c(.data$home, .data$away),
                                  names_to = NULL, values_to = "team")
  list_teams <- dplyr::filter(pivoted, purrr::map_lgl(.data$team, is.list))
  message("[week_starter] rows after pivot/is.list: ", nrow(list_teams))

  if (nrow(list_teams) > 0) {
    message("[week_starter] first team fields: ",
            paste(names(list_teams$team[[1]]), collapse=", "))
  }

  if (nrow(list_teams) == 0) return(tibble::tibble())

  hoisted <- tidyr::hoist(list_teams, "team",
                           "starting_lineup" = "rosterForCurrentScoringPeriod",
                           "franchise_id" = "teamId") %>%
    dplyr::select(-"team", -"x")

  message("[week_starter] is.list(starting_lineup): ",
          sum(purrr::map_lgl(hoisted$starting_lineup, is.list)),
          " / ", nrow(hoisted))

  raw <- dplyr::filter(hoisted, purrr::map_lgl(.data$starting_lineup, is.list))

  if (nrow(raw) == 0) return(tibble::tibble())

  week_scores <- raw %>%
    tidyr::hoist("starting_lineup", "franchise_score" = "appliedStatTotal", "entries") %>%
    tidyr::unnest_longer("entries") %>%
    dplyr::filter(purrr::map_lgl(.data$entries, is.list))

  if (nrow(week_scores) == 0) return(tibble::tibble())

  week_scores <- week_scores %>%
    tidyr::hoist("entries", "player_id" = "playerId", "lineup_id" = "lineupSlotId", "player_data" = "playerPoolEntry") %>%
    dplyr::filter(purrr::map_lgl(.data$player_data, is.list))

  if (nrow(week_scores) == 0) return(tibble::tibble())

  week_scores <- week_scores %>%
    tidyr::hoist("player_data", "player_score" = "appliedStatTotal", "player") %>%
    dplyr::select(-"player_data") %>%
    dplyr::filter(purrr::map_lgl(.data$player, is.list))

  if (nrow(week_scores) == 0) return(tibble::tibble())

  week_scores <- week_scores %>%
    tidyr::hoist("player",
                 "eligible_lineup_slots" = "eligibleSlots",
                 "player_name" = "fullName",
                 "pos" = "defaultPositionId",
                 "team" = "proTeamId",
                 ) %>%
    dplyr::mutate(
      projected_score = purrr::map_dbl(.data$player,
                                       ~.x %>%
                                         purrr::pluck("stats",
                                                      2, # assume stats list col returns actual as first list and projected as second
                                                      "appliedTotal",
                                                      .default = NA_real_) %>%
                                         round(1)),
      player = NULL)

  return(week_scores)
}
