#### get_weekly_points ####

#' Get Hitting, Pitching, and Total Points by Team for a Given Week
#'
#' Returns a tidy summary of fantasy points broken down into hitting and
#' pitching contributions for each team in the specified week(s). Team owner
#' names are resolved automatically from the connection.
#'
#' Hitting and pitching points are calculated using
#' \code{\link{espn_hitter_points}} and \code{\link{espn_pitcher_points}}
#' respectively, so this function currently requires an ESPN connection.
#'
#' @param conn A connection object created by \code{\link{espn_connect}}.
#' @param week Integer (or integer vector). The scoring week(s) to retrieve.
#'
#' @return A tibble with one row per team per week and columns:
#'   \describe{
#'     \item{week}{Scoring week number.}
#'     \item{user_name}{Owner name (falls back to \code{franchise_name} when
#'       owner info is unavailable).}
#'     \item{hitting_points}{Total fantasy points from hitting lineup slots.}
#'     \item{pitching_points}{Total fantasy points from pitching lineup slots.}
#'     \item{total_points}{Sum of hitting and pitching points.}
#'   }
#'
#' @examples
#' \donttest{
#' try({
#'   conn <- espn_connect(season = 2026, league_id = 12345)
#'   get_weekly_points(conn, week = 1)
#' })
#' }
#'
#' @export
get_weekly_points <- function(conn, week) {
  franchises <- ff_franchises(conn) %>%
    dplyr::mutate(
      franchise_id = as.character(.data$franchise_id),
      user_name    = dplyr::coalesce(.data$user_name, .data$franchise_name)
    ) %>%
    dplyr::select("franchise_id", "user_name")

  hitting  <- espn_hitter_points(conn, weeks = week) %>%
    dplyr::mutate(franchise_id = as.character(.data$franchise_id)) %>%
    dplyr::select("week", "franchise_id", "hitting_points" = "hitter_points")

  pitching <- espn_pitcher_points(conn, weeks = week) %>%
    dplyr::mutate(franchise_id = as.character(.data$franchise_id)) %>%
    dplyr::select("week", "franchise_id", "pitching_points" = "pitcher_points")

  hitting %>%
    dplyr::left_join(pitching,   by = c("week", "franchise_id")) %>%
    dplyr::left_join(franchises, by = "franchise_id") %>%
    dplyr::mutate(total_points = .data$hitting_points + .data$pitching_points) %>%
    dplyr::select("week", "user_name", "hitting_points", "pitching_points", "total_points") %>%
    dplyr::arrange(.data$week, dplyr::desc(.data$total_points))
}
