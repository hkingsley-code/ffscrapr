#### BASEBALL STATS IMPORT ####

#' Import baseball player stats
#'
#' Fetches player stats using the baseballr package.
#'
#' @param seasons The seasons to return as a numeric vector.
#' @param type One of "batting" or "pitching"
#'
#' @examples
#' \donttest{
#' try( # try only shown here because sometimes CRAN checks are weird
#'   baseball_stats(seasons = 2024, type = "batting")
#' )
#' }
#'
#' @return Stats for batters or pitchers for the specified seasons
#'
#' @export
baseball_stats <- function(seasons,
                           type = c("batting", "pitching")) {

  type <- match.arg(type)

  if (!requireNamespace("baseballr", quietly = TRUE)) {
    stop("Package 'baseballr' is required for this function. Install it with install.packages('baseballr')")
  }

  df_stats <- purrr::map_dfr(seasons, function(season) {
    if (type == "batting") {
      baseballr::fg_batter_leaders(x = season, y = season) %>%
        dplyr::mutate(season = season)
    } else {
      baseballr::fg_pitcher_leaders(x = season, y = season) %>%
        dplyr::mutate(season = season)
    }
  })

  return(df_stats)
}
