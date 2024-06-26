% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_generics.R, R/espn_scoringhistory.R,
%   R/flea_scoringhistory.R, R/mfl_scoringhistory.R, R/sleeper_scoringhistory.R,
%   R/template_scoringhistory.R
\name{ff_scoringhistory}
\alias{ff_scoringhistory}
\alias{ff_scoringhistory.espn_conn}
\alias{ff_scoringhistory.flea_conn}
\alias{ff_scoringhistory.mfl_conn}
\alias{ff_scoringhistory.sleeper_conn}
\alias{ff_scoringhistory.template_conn}
\title{Get League-Specific Scoring History}
\usage{
ff_scoringhistory(conn, season, ...)

\method{ff_scoringhistory}{espn_conn}(conn, season = 1999:2020, ...)

\method{ff_scoringhistory}{flea_conn}(conn, season = 1999:2020, ...)

\method{ff_scoringhistory}{mfl_conn}(conn, season = 1999:2020, ...)

\method{ff_scoringhistory}{sleeper_conn}(conn, season = 1999:2020, ...)

\method{ff_scoringhistory}{template_conn}(conn, season = 1999:2020, ...)
}
\arguments{
\item{conn}{a conn object created by \code{ff_connect()}}

\item{season}{season a numeric vector of seasons (earliest available year is 1999)}

\item{...}{other arguments}
}
\value{
A tidy dataframe of weekly fantasy scoring data, one row per player per week
}
\description{
(Experimental!) This function reads your league's ff_scoring rules and maps them to nflfastr week-level data.
Not all of the scoring rules from your league may have nflfastr equivalents, but most of the common ones are available!
}
\section{Methods (by class)}{
\itemize{
\item \code{espn_conn}: ESPN: returns scoring history in a flat table, one row per player per week.

\item \code{flea_conn}: Fleaflicker: returns scoring history in a flat table, one row per player per week.

\item \code{mfl_conn}: MFL: returns scoring history in a flat table, one row per player per week.

\item \code{sleeper_conn}: Sleeper: returns scoring history in a flat table, one row per player per week.

\item \code{template_conn}: template: returns scoring history in a flat table, one row per player per week.
}}

\examples{
\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  conn <- espn_connect(season = 2020, league_id = 899513)
  ff_scoringhistory(conn, season = 2020)
}) # end try
}

\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  conn <- fleaflicker_connect(2020, 312861)
  ff_scoringhistory(conn, season = 2020)
}) # end try
}

\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  ssb_conn <- ff_connect(platform = "mfl", league_id = 54040, season = 2020)
  ff_scoringhistory(ssb_conn, season = 2020)
}) # end try
}

\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  conn <- ff_connect(platform = "sleeper", league_id = "522458773317046272", season = 2020)
  ff_scoringhistory(conn, season = 2020)
}) # end try
}

\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  template_conn <- ff_template(scoring_type = "sfb11", roster_type = "sfb11")
  ff_scoringhistory(template_conn, season = 2020)
}) # end try
}

}
\seealso{
\url{https://www.nflfastr.com/reference/load_player_stats.html}
}
