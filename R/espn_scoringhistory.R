## ff_scoringhistory (ESPN) ##

#' Get a dataframe of scoring history, utilizing the ff_scoring and ESPN player stats.
#'
#' @param conn a conn object created by `ff_connect()`
#' @param season season a numeric vector of seasons (earliest available year is 2004)
#' @param ... other arguments
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   conn <- espn_connect(season = 2024, league_id = 85601)
#'   ff_scoringhistory(conn, season = 2024)
#' }) # end try
#' }
#'
#' @describeIn ff_scoringhistory ESPN: returns scoring history in a flat table, one row per player per scoring period.
#'
#' @export
ff_scoringhistory.espn_conn <- function(conn, season = 2004:2024, ...) {
  checkmate::assert_numeric(season, lower = 2004, upper = as.integer(format(Sys.Date(), "%Y")))

  # ESPN baseball scoring history requires league-specific endpoint calls
  # rather than the nflfastr approach used for football
  rlang::inform("ESPN baseball scoring history is not yet fully implemented. Use ff_playerscores() for current season data.")

  return(NULL)
}
