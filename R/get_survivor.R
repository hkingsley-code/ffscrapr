#### get_survivor ####

#' Get Survivor Pool Result for a Given Week
#'
#' Finds the team with the lowest score in a given fantasy baseball scoring
#' week, excluding any teams that have already been eliminated in previous
#' weeks (by having the lowest score in their respective week). Also returns
#' all teams still alive after the given week.
#'
#' The function processes each week from 1 through \code{week} in order,
#' cumulatively tracking eliminated teams. In each week, only teams not yet
#' eliminated compete, and the lowest-scoring team among them is eliminated.
#'
#' Team names are resolved in this priority order:
#' \enumerate{
#'   \item \code{franchise_name} already present in \code{schedule}
#'     (Fleaflicker includes this automatically).
#'   \item \code{conn} supplied — \code{\link{ff_franchises}} is called
#'     internally to look up names.
#'   \item \code{franchises} tibble supplied directly (output of
#'     \code{\link{ff_franchises}}).
#'   \item \code{franchise_id} used as a fallback when none of the above
#'     are available.
#' }
#'
#' @param schedule A tibble returned by \code{\link{ff_schedule}}, containing
#'   at minimum the columns \code{week}, \code{franchise_id}, and
#'   \code{franchise_score}. Weeks with \code{NA} scores (unplayed games) are
#'   automatically excluded from elimination consideration.
#' @param week Integer. The scoring week number to evaluate.
#' @param conn Optional. A connection object created by \code{\link{ff_connect}}
#'   (or a platform-specific connect function). When supplied, franchise names
#'   are fetched automatically via \code{\link{ff_franchises}}.
#' @param franchises Optional. A tibble returned by \code{\link{ff_franchises}},
#'   used to look up \code{franchise_name}. Ignored if \code{conn} is supplied
#'   or if \code{schedule} already contains a \code{franchise_name} column.
#'
#' @return If teams remain to be eliminated in \code{week}: a named list with
#'   \describe{
#'     \item{loser}{A one-row tibble with \code{franchise_name} (or
#'       \code{franchise_id} if names are unavailable) and
#'       \code{franchise_score} of the team eliminated in \code{week}.}
#'     \item{survivors}{A tibble of team names (or IDs) for all teams still
#'       alive after \code{week}.}
#'   }
#'   If no teams are left to compete in any week (all have previously been
#'   eliminated): the string \code{"all teams have been eliminated"}.
#'
#' @examples
#' \donttest{
#' try({
#'   conn  <- ff_connect(platform = "espn", league_id = 12345, season = 2023)
#'   sched <- ff_schedule(conn)
#'
#'   # Simplest — pass conn and names are fetched automatically
#'   get_survivor(sched, week = 3, conn = conn)
#' })
#' }
#'
#' @export
get_survivor <- function(schedule, week, conn = NULL, franchises = NULL) {
  # Normalise franchise_id to character so joins work regardless of platform
  # (ESPN returns integer from map_int; other platforms may return double or character)
  schedule <- dplyr::mutate(schedule, franchise_id = as.character(.data$franchise_id))

  # Fetch franchise names from connection if provided
  if (!is.null(conn) && !"franchise_name" %in% names(schedule)) {
    franchises <- ff_franchises(conn)
  }

  # Join franchise_name if not already in schedule
  if (!"franchise_name" %in% names(schedule) && !is.null(franchises)) {
    schedule <- schedule %>%
      dplyr::left_join(
        franchises %>%
          dplyr::select("franchise_id", "franchise_name") %>%
          dplyr::mutate(franchise_id = as.character(.data$franchise_id)),
        by = "franchise_id"
      )
  }

  has_name <- "franchise_name" %in% names(schedule)

  eliminated_ids <- character(0)
  all_ids        <- unique(schedule$franchise_id)
  loser_row      <- NULL

  for (w in seq_len(week)) {
    alive_scores <- schedule %>%
      dplyr::filter(
        .data$week == w,
        !.data$franchise_id %in% eliminated_ids,
        !is.na(.data$franchise_score)
      )

    if (nrow(alive_scores) == 0) {
      return("all teams have been eliminated")
    }

    loser_row <- alive_scores %>%
      dplyr::arrange(.data$franchise_score) %>%
      dplyr::slice(1)

    eliminated_ids <- c(eliminated_ids, loser_row$franchise_id)
  }

  # Build loser output — prefer name over id
  loser_out <- loser_row %>%
    dplyr::select(dplyr::any_of(c("franchise_name", "franchise_id", "franchise_score"))) %>%
    dplyr::select(-dplyr::any_of(if (has_name) "franchise_id" else character(0)))

  # Build survivors
  survivor_ids <- setdiff(all_ids, eliminated_ids)

  if (has_name) {
    name_lookup <- schedule %>%
      dplyr::distinct(.data$franchise_id, .data$franchise_name)

    survivors <- tibble::tibble(franchise_id = survivor_ids) %>%
      dplyr::left_join(name_lookup, by = "franchise_id") %>%
      dplyr::select("franchise_name")
  } else {
    survivors <- tibble::tibble(franchise_id = survivor_ids)
  }

  list(loser = loser_out, survivors = survivors)
}
