#### ff_standings (ESPN) ####

#' Get a dataframe of league standings
#'
#' @param conn the connection object created by `ff_connect()`
#' @param ... other arguments (for other platforms)
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   espn_conn <- espn_connect(season = 2020, league_id = 899513)
#'   ff_standings(espn_conn)
#' }) # end try
#' }
#'
#' @describeIn ff_standings ESPN: returns standings and points data.
#'
#' @export
ff_standings.espn_conn <- function(conn, ...) {
  team_endpoint <-
    espn_getendpoint(conn, view = "mTeam") %>%
    purrr::pluck("content")

  teams_raw <- purrr::pluck(team_endpoint, "teams")

  if (is.null(teams_raw) || length(teams_raw) == 0) {
    warning(
      glue::glue(
        "ESPN could not retrieve standings for {conn$season} league {conn$league_id}. ",
        "If this is a private league, supply espn_s2 and swid to espn_connect()."
      ),
      call. = FALSE
    )
    return(NULL)
  }

  standings_init <- teams_raw %>%
    tibble::tibble() %>%
    tidyr::hoist(
      .col = 1,
      "franchise_id" = "id",
      "league_rank" = "rankCalculatedFinal",
      "record" = "record"
    ) %>%
    dplyr::select(
      dplyr::any_of(c(
        "franchise_id",
        "league_rank",
        "record"
      ))
    )

  records <-
    standings_init %>%
    dplyr::select(.data$record) %>%
    tidyr::hoist(
      "record",
      "overall"
    ) %>%
    dplyr::select(-.data$record) %>%
    tidyr::hoist(
      "overall",
      "h2h_wins" = "wins",
      "h2h_losses" = "losses",
      "h2h_ties" = "ties",
      "h2h_winpct" = "percentage",
      "points_for" = "pointsFor",
      "points_against" = "pointsAgainst",
    ) %>%
    dplyr::select(-.data$overall)

  allplay <- ff_schedule(conn) %>%
    .add_allplay()

  franchise_names <- ff_franchises(conn) %>%
    dplyr::select(dplyr::any_of(c(
      "franchise_id", "franchise_name", "division_id", "division_name"
    )))

  standings <-
    dplyr::bind_cols(
      standings_init %>% dplyr::select(-.data$record),
      records
    ) %>%
    dplyr::left_join(allplay, by = c("franchise_id")) %>%
    dplyr::left_join(x = franchise_names, by = c("franchise_id"))

  return(standings)
}
