####  Flea ff_starter_positions ####

#' Get starters and bench
#'
#' @param conn the list object created by `ff_connect()`
#' @param ... other arguments (currently unused)
#'
#' @describeIn ff_starter_positions Fleaflicker: returns minimum and maximum starters for each player position.
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   conn <- fleaflicker_connect(season = 2020, league_id = 206154)
#'   ff_starter_positions(conn)
#' }) # end try
#' }
#'
#' @export
ff_starter_positions.flea_conn <- function(conn, ...) {
  x <- fleaflicker_getendpoint("FetchLeagueRules",
    sport = "MLB",
    league_id = conn$league_id
  ) %>%
    purrr::pluck("content", "rosterPositions") %>%
    tibble::tibble() %>%
    tidyr::hoist(1, "label", "group", "eligibility", "start") %>%
    dplyr::select("label", "group", "eligibility", min = "start") %>%
    dplyr::filter(.data$group == "START") %>%
    dplyr::mutate_at("min", tidyr::replace_na, 0) %>%
    dplyr::mutate(
      total_starters = sum(min),
      batter_starters = sum(stringr::str_detect(.data$label, "C|1B|2B|3B|SS|OF|DH|UTIL") * .data$min),
      pitcher_starters = sum(stringr::str_detect(.data$label, "SP|RP|P") * .data$min)
    ) %>%
    tidyr::unnest_longer("eligibility") %>%
    dplyr::group_by(.data$eligibility, .data$total_starters, .data$batter_starters, .data$pitcher_starters) %>%
    dplyr::summarise(
      pos_min = sum(stringr::str_detect(.data$label, "/", negate = TRUE) * .data$min, na.rm = TRUE),
      pos_max = sum(.data$min, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$pos_max != 0) %>%
    dplyr::select(
      pos = "eligibility",
      min = "pos_min",
      max = "pos_max",
      "batter_starters",
      "pitcher_starters",
      "total_starters"
    )


  return(x)
}
