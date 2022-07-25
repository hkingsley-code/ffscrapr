## ff_scoring (ESPN) ##

#' Get a dataframe of scoring settings
#'
#' @param conn a conn object created by `ff_connect()`
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   conn <- espn_connect(season = 2020, league_id = 899513)
#'   ff_scoring(conn)
#' }) # end try
#' }
#'
#' @describeIn ff_scoring ESPN: returns scoring settings in a flat table, override positions have their own scoring.
#'
#' @export
ff_scoring.espn_conn <- function(conn) {
  scoring_rules <-
    espn_getendpoint(conn, view = "mSettings") %>%
    purrr::pluck("content", "settings", "scoringSettings", "scoringItems") %>%
    tibble::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::mutate(stat = .espn_stat_map()[as.character(.data$statId)] %>% unname())

  main_stats <-
    scoring_rules %>%
    tidyr::expand_grid(pos = c("C","1B","2B","SS","3B","OF","SP","RP","UTIL","IF")) %>%
    dplyr::select(
      "pos",
      "points",
      "stat_id" = "statId",
      "stat_name" = "stat"
    )

  return(main_stats)
}
