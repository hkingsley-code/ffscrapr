#### ESPN LEAGUE SUMMARY ####

#' Get a summary of common league settings
#'
#' @param conn the connection object created by `ff_connect()`
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'
#'   conn <- espn_connect(season = 2024, league_id = 85601)
#'
#'   ff_league(conn)
#' }) # end try
#' }
#'
#' @describeIn ff_league ESPN: returns a summary of league features.
#'
#' @export
ff_league.espn_conn <- function(conn) {
  league_endpoint <-
    espn_getendpoint(
      conn = conn,
      view = "mSettings"
    )

  franchise_count <- league_endpoint$content$settings$size
  roster_size <- .espn_roster_size(league_endpoint)
  player_copies <- 1

  tibble::tibble(
    league_id = conn$league_id,
    league_name = league_endpoint$content$settings$name,
    season = as.integer(conn$season),
    league_type = .espn_is_keeper(league_endpoint),
    franchise_count = franchise_count,
    scoring_type = .espn_scoring_type(league_endpoint),
    scoring_flags = .espn_scoring_flags(league_endpoint),
    salary_cap = FALSE,
    player_copies = player_copies,
    years_active = .espn_leaguehistory(conn, league_endpoint),
    roster_size = roster_size,
    league_depth = roster_size * franchise_count / player_copies,
    keeper_count = league_endpoint$content$settings$draftSettings$keeperCount
  )
}

#' @noRd
.espn_scoring_type <- function(league_endpoint) {
  scoring_type_id <- purrr::pluck(league_endpoint, "content", "settings", "scoringSettings", "scoringType", .default = NA)

  dplyr::case_when(
    is.na(scoring_type_id) ~ "unknown",
    TRUE ~ as.character(scoring_type_id)
  )
}

#' @noRd
.espn_leaguehistory <- function(conn, league_endpoint) {
  start_year <- utils::head(league_endpoint$content$status$previousSeasons, 1) %>% unlist()

  paste0(start_year, "-", conn$season)
}

#' @noRd
.espn_scoring_flags <- function(league_endpoint) {
  scoring_items <- league_endpoint %>%
    purrr::pluck("content", "settings", "scoringSettings", "scoringItems")

  if (is.null(scoring_items) || length(scoring_items) == 0) return("")

  stat_map <- .espn_stat_map()
  stats_used <- scoring_items %>%
    purrr::map(`[`, c("statId", "points")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      stat_name = .espn_stat_map()[as.character(.data$statId)]
    )

  flags <- list()

  # Check for points-based scoring
  if (nrow(stats_used) > 0) {
    flags <- append(flags, "points")
  }

  paste(flags[!is.na(flags) & !is.null(flags)], collapse = ", ")
}

#' @noRd
.espn_roster_size <- function(league_endpoint) {
  roster_size <- league_endpoint$content$settings$rosterSettings$lineupSlotCounts %>%
    purrr::map_int(~.x) %>%
    sum()
  roster_size
}

.espn_is_keeper <- function(league_endpoint) {
  x <- purrr::pluck(league_endpoint, "content", "settings", "draftSettings", "keeperCount")

  dplyr::case_when(
    x == 0 ~ "redraft",
    TRUE ~ "keeper"
  )
}
