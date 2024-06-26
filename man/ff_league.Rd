% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_generics.R, R/espn_league.R, R/flea_league.R,
%   R/mfl_league.R, R/sleeper_league.R
\name{ff_league}
\alias{ff_league}
\alias{ff_league.espn_conn}
\alias{ff_league.flea_conn}
\alias{ff_league.mfl_conn}
\alias{ff_league.sleeper_conn}
\title{Get League Summary}
\usage{
ff_league(conn)

\method{ff_league}{espn_conn}(conn)

\method{ff_league}{flea_conn}(conn)

\method{ff_league}{mfl_conn}(conn)

\method{ff_league}{sleeper_conn}(conn)
}
\arguments{
\item{conn}{the connection object created by \code{ff_connect()}}
}
\value{
A one-row summary of each league's main features.
}
\description{
This function returns a tidy dataframe of common league settings, including details like "1QB" or "2QB/SF", scoring, best ball, team count, IDP etc. This is potentially useful in summarising the features of multiple leagues.
}
\section{Methods (by class)}{
\itemize{
\item \code{espn_conn}: ESPN: returns a summary of league features.

\item \code{flea_conn}: Flea: returns a summary of league features.

\item \code{mfl_conn}: MFL: returns a summary of league features.

\item \code{sleeper_conn}: Sleeper: returns a summary of league features.
}}

\examples{
\donttest{
try({ # try only shown here because sometimes CRAN checks are weird

  conn <- espn_connect(season = 2020, league_id = 899513)

  ff_league(conn)
}) # end try
}

\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  conn <- fleaflicker_connect(2020, 206154)
  ff_league(conn)
}) # end try
}

\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  ssb_conn <- ff_connect(platform = "mfl", league_id = 22627, season = 2021)
  ff_league(ssb_conn)
}) # end try
}

\donttest{
try({ # try only shown here because sometimes CRAN checks are weird
  jml_conn <- ff_connect(platform = "sleeper", league_id = "522458773317046272", season = 2020)
  ff_league(jml_conn)
}) # end try
}
}
