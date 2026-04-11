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
#' @param schedule A tibble returned by \code{\link{ff_schedule}}, containing
#'   at minimum the columns \code{week}, \code{franchise_id}, and
#'   \code{franchise_score}. Weeks with \code{NA} scores (unplayed games) are
#'   automatically excluded from elimination consideration.
#' @param week Integer. The scoring week number to evaluate.
#'
#' @return If teams remain to be eliminated in \code{week}: a named list with
#'   \describe{
#'     \item{loser}{A one-row tibble with \code{franchise_id} and
#'       \code{franchise_score} of the team eliminated in \code{week}.}
#'     \item{survivors}{A tibble of \code{franchise_id} values for all teams
#'       still alive after \code{week} (i.e., have not had the lowest score in
#'       any week from 1 through \code{week}).}
#'   }
#'   If no teams are left to compete in any week (all have previously been
#'   eliminated): the string \code{"all teams have been eliminated"}.
#'
#' @examples
#' \donttest{
#' try({
#'   conn <- ff_connect(platform = "espn", league_id = 12345, season = 2023)
#'   sched <- ff_schedule(conn)
#'   get_survivor(sched, week = 3)
#' })
#' }
#'
#' @export
get_survivor <- function(schedule, week) {
  eliminated <- character(0)
  all_ids <- unique(schedule$franchise_id)

  loser_row <- NULL

  for (w in seq_len(week)) {
    alive_scores <- schedule %>%
      dplyr::filter(
        .data$week == w,
        !.data$franchise_id %in% eliminated,
        !is.na(.data$franchise_score)
      )

    if (nrow(alive_scores) == 0) {
      return("all teams have been eliminated")
    }

    loser_row <- alive_scores %>%
      dplyr::arrange(.data$franchise_score) %>%
      dplyr::slice(1) %>%
      dplyr::select("franchise_id", "franchise_score")

    eliminated <- c(eliminated, loser_row$franchise_id)
  }

  survivors <- tibble::tibble(
    franchise_id = setdiff(all_ids, eliminated)
  )

  list(loser = loser_row, survivors = survivors)
}
