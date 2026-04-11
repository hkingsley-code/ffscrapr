#### Sleeper ff_starter_positions ####

#' Get starters and bench
#'
#' @param conn the list object created by `ff_connect()`
#' @param ... other arguments (currently unused)
#'
#' @describeIn ff_starter_positions Sleeper: returns minimum and maximum starters for each player position.
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   jml_conn <- sleeper_connect(league_id = "652718526494253056", season = 2021)
#'   ff_starter_positions(jml_conn)
#' }) # end try
#' }
#'
#' @export
ff_starter_positions.sleeper_conn <- function(conn, ...) {
  df_positions <- sleeper_getendpoint(glue::glue("league/{conn$league_id}")) %>%
    purrr::pluck("content", "roster_positions") %>%
    tibble::tibble() %>%
    purrr::set_names("pos") %>%
    dplyr::filter(.data$pos != "BN") %>%
    dplyr::group_by(.data$pos) %>%
    dplyr::count(name = "min") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      total_starters = sum(.data$min, na.rm = TRUE),
      pos = purrr::map_chr(.data$pos, unlist)
    )

  util <- ifelse(length(df_positions$min[df_positions$pos == "UTIL"]) == 0, 0, df_positions$min[df_positions$pos == "UTIL"])
  inf_flex <- ifelse(length(df_positions$min[df_positions$pos == "IF"]) == 0, 0, df_positions$min[df_positions$pos == "IF"])
  mi <- ifelse(length(df_positions$min[df_positions$pos == "MI"]) == 0, 0, df_positions$min[df_positions$pos == "MI"])
  ci <- ifelse(length(df_positions$min[df_positions$pos == "CI"]) == 0, 0, df_positions$min[df_positions$pos == "CI"])

  df_positions %>%
    dplyr::mutate(
      max = dplyr::case_when(
        .data$pos == "C" ~ as.integer(.data$min + util),
        .data$pos == "1B" ~ as.integer(.data$min + ci + inf_flex + util),
        .data$pos == "2B" ~ as.integer(.data$min + mi + inf_flex + util),
        .data$pos == "3B" ~ as.integer(.data$min + ci + inf_flex + util),
        .data$pos == "SS" ~ as.integer(.data$min + mi + inf_flex + util),
        .data$pos == "OF" ~ as.integer(.data$min + util),
        .data$pos == "DH" ~ as.integer(.data$min + util),
        .data$pos %in% c("SP", "RP") ~ as.integer(.data$min),
        TRUE ~ as.integer(.data$min)
      ),
      total_starters = sum(.data$min, na.rm = TRUE),
      batter_starters = sum(
        .data$pos %in% c("C", "1B", "2B", "3B", "SS", "OF", "DH", "UTIL", "IF", "MI", "CI") * .data$min,
        na.rm = TRUE
      ),
      pitcher_starters = sum(.data$pos %in% c("SP", "RP", "P") * .data$min, na.rm = TRUE)
    ) %>%
    dplyr::filter(stringr::str_detect(.data$pos, "UTIL|^IF$|^MI$|^CI$", negate = TRUE)) %>%
    dplyr::select(
      "pos", "min", "max", "batter_starters", "pitcher_starters", "total_starters"
    )
}
