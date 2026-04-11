#### ESPN ff_starter_positions ####

#' Get starters and bench
#'
#' @param conn the list object created by `ff_connect()`
#' @param ... other arguments (currently unused)
#'
#' @describeIn ff_starter_positions ESPN: returns min/max starters for each main player position
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   conn <- espn_connect(season = 2024, league_id = 85601)
#'   ff_starter_positions(conn)
#' }) # end try
#' }
#'
#' @export
ff_starter_positions.espn_conn <- function(conn, ...) {
  l_s <- espn_getendpoint(conn, view = "mSettings") %>%
    purrr::pluck("content", "settings", "rosterSettings", "lineupSlotCounts") %>%
    tibble::enframe(name = "lineup_id", value = "count") %>%
    dplyr::mutate(pos = .espn_lineupslot_map()[as.character(.data$lineup_id)]) %>%
    tidyr::unnest(c("pos", "count")) %>%
    dplyr::mutate(
      min = ifelse(.data$pos %in% c(
        "C", "1B", "2B", "3B", "SS", "OF",
        "LF", "CF", "RF", "DH",
        "SP", "RP", "P"
      ), .data$count, NA_integer_),
      batter_starters = sum(.data$min * stringr::str_detect(.data$pos, "C|1B|2B|3B|SS|OF|LF|CF|RF|DH"), na.rm = TRUE),
      pitcher_starters = sum(.data$min * stringr::str_detect(.data$pos, "^SP$|^RP$|^P$"), na.rm = TRUE),
      total_starters = .data$batter_starters + .data$pitcher_starters
    )

  util <- if (length(l_s$count[l_s$pos == "UTIL"]) > 0) l_s$count[l_s$pos == "UTIL"] else 0
  mi <- if (length(l_s$count[l_s$pos == "2B/SS"]) > 0) l_s$count[l_s$pos == "2B/SS"] else 0
  ci <- if (length(l_s$count[l_s$pos == "1B/3B"]) > 0) l_s$count[l_s$pos == "1B/3B"] else 0
  inf <- if (length(l_s$count[l_s$pos == "IF"]) > 0) l_s$count[l_s$pos == "IF"] else 0

  l_s %>%
    dplyr::mutate(
      max = dplyr::case_when(
        .data$pos == "C" ~ .data$min + util,
        .data$pos == "1B" ~ .data$min + ci + inf + util,
        .data$pos == "2B" ~ .data$min + mi + inf + util,
        .data$pos == "3B" ~ .data$min + ci + inf + util,
        .data$pos == "SS" ~ .data$min + mi + inf + util,
        .data$pos == "OF" ~ .data$min + util,
        .data$pos == "DH" ~ .data$min + util,
        .data$pos == "SP" ~ .data$min,
        .data$pos == "RP" ~ .data$min,
        TRUE ~ .data$min
      ),
    ) %>%
    dplyr::filter(!is.na(.data$min), .data$min > 0) %>%
    dplyr::select(
      "pos",
      "min",
      "max",
      dplyr::contains("_starters")
    )
}
