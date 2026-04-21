#### get_survivor ####

#' Get Survivor Pool Result for a Given Week
#'
#' Fetches schedule and franchise data for the supplied connection, then finds
#' the team with the lowest score in a given fantasy baseball scoring week,
#' excluding any teams that have already been eliminated in previous weeks (by
#' having the lowest score in their respective week). Also returns all teams
#' still alive after the given week.
#'
#' The function processes each week from 1 through \code{week} in order,
#' cumulatively tracking eliminated teams. In each week, only teams not yet
#' eliminated compete, and the lowest-scoring team among them is eliminated.
#'
#' The display name used in output is the owner's \code{user_name} where
#' available (ESPN, Sleeper, Fleaflicker). For platforms that do not return
#' owner names (MFL), \code{franchise_name} is used as a fallback.
#'
#' @param conn A connection object created by \code{\link{ff_connect}} (or a
#'   platform-specific connect function such as \code{\link{espn_connect}}).
#' @param week Integer. The scoring week number to evaluate.
#'
#' @return If teams remain to be eliminated in \code{week}: a named list with
#'   \describe{
#'     \item{loser}{A one-row tibble with \code{user_name} and
#'       \code{franchise_score} of the team eliminated in \code{week}.}
#'     \item{survivors}{A tibble of \code{user_name} values for all teams
#'       still alive after \code{week}.}
#'   }
#'   If no teams are left to compete (all have previously been eliminated):
#'   the string \code{"all teams have been eliminated"}.
#'
#' @examples
#' \donttest{
#' try({
#'   conn <- ff_connect(platform = "espn", league_id = 12345, season = 2023)
#'   get_survivor(conn, week = 1)
#' })
#' }
#'
#' @export
get_survivor <- function(conn, week) {
  if (is.data.frame(conn)) {
    stop(
      "The first argument to get_survivor() must be a connection object (from ff_connect() ",
      "or espn_connect() etc.), not a schedule tibble.\n",
      "Usage: get_survivor(conn, week = ", week, ")",
      call. = FALSE
    )
  }

  schedule   <- ff_schedule(conn)
  franchises <- ff_franchises(conn)

  .get_survivor_logic(schedule, franchises, week)
}

#' Internal survivor logic
#'
#' Separated from \code{get_survivor} so it can be tested without a live
#' connection. Expects \code{schedule} and \code{franchises} to already be
#' fetched.
#'
#' @param schedule Tibble from \code{ff_schedule()}.
#' @param franchises Tibble from \code{ff_franchises()}.
#' @param week Integer week to evaluate.
#'
#' @keywords internal
.get_survivor_logic <- function(schedule, franchises, week) {
  # Resolve display name: prefer user_name (owner), fall back to franchise_name
  # MFL does not return user_name so coalesce ensures a usable value
  name_lookup <- franchises %>%
    dplyr::mutate(
      franchise_id = as.character(.data$franchise_id),
      user_name    = dplyr::coalesce(.data$user_name, .data$franchise_name)
    ) %>%
    dplyr::select("franchise_id", "user_name")

  # Normalise franchise_id to character — ESPN returns integer from map_int,
  # other platforms may return double or character
  schedule <- schedule %>%
    dplyr::mutate(franchise_id = as.character(.data$franchise_id)) %>%
    dplyr::left_join(name_lookup, by = "franchise_id")

  eliminated_ids <- character(0)
  all_ids        <- unique(schedule$franchise_id)
  loser_row      <- NULL

  for (w in seq_len(week)) {
    alive <- schedule %>%
      dplyr::filter(
        .data$week == w,
        !.data$franchise_id %in% eliminated_ids,
        !is.na(.data$franchise_score)
      )

    if (nrow(alive) == 0) {
      return("all teams have been eliminated")
    }

    loser_row      <- alive %>% dplyr::arrange(.data$franchise_score) %>% dplyr::slice(1)
    eliminated_ids <- c(eliminated_ids, loser_row$franchise_id)
  }

  name_map <- dplyr::distinct(schedule, .data$franchise_id, .data$user_name)

  survivors <- tibble::tibble(franchise_id = setdiff(all_ids, eliminated_ids)) %>%
    dplyr::left_join(name_map, by = "franchise_id") %>%
    dplyr::select("user_name")

  list(
    loser     = dplyr::select(loser_row, "user_name", "franchise_score"),
    survivors = survivors
  )
}
