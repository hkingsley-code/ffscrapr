#' Default `conn` objects
#'
#' This function creates a connection to a few league templates, and can be used instead of a real conn object in the following functions: `ff_scoring()`, `ff_scoringhistory()`, `ff_starterpositions()`.
#'
#' Scoring types defined here are:
#'
#' - `standard`: Standard 5x5 roto scoring (R, HR, RBI, SB, AVG for batters; W, SV, K, ERA, WHIP for pitchers)
#' - `points`: Points-based scoring with common category values
#' - `h2h_cat`: Head-to-head categories scoring
#'
#' Roster settings defined here are:
#'
#' - `standard`: Starts 1 C, 1 1B, 1 2B, 1 3B, 1 SS, 3 OF, 1 UTIL, 2 SP, 2 RP, 2 P
#' - `deep`: Starts 2 C, 1 1B, 1 2B, 1 3B, 1 SS, 5 OF, 1 UTIL, 3 SP, 2 RP, 2 P
#'
#' @param scoring_type One of c("standard", "points", "h2h_cat")
#' @param roster_type One of c("standard", "deep")
#'
#' @return a connection object that can be used with `ff_scoring()`, `ff_scoringhistory()`, and `ff_starterpositions()`
#' @export

ff_template <- function(scoring_type = c("standard", "points", "h2h_cat"),
                        roster_type = c("standard", "deep")) {
  scoring_type <- match.arg(scoring_type)
  roster_type <- match.arg(roster_type)

  out <- structure(
    list(
      scoring_type = scoring_type,
      roster_type = roster_type
    ),
    class = "template_conn"
  )
  return(out)
}

# nocov start
#' @noRd
#' @export
print.template_conn <- function(x, ...) {
  cat("<Default league: ", x$scoring_type, " - ", x$roster_type, ">\n", sep = "")
  invisible(x)
}
# nocov end
