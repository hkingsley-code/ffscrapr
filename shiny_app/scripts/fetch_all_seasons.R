# fetch_all_seasons.R
#
# Run this script ONCE (offline) to populate shiny_app/data/ with cached .rds files.
# Re-run at the end of each season, or weekly during an active season.
#
# Usage:
#   Rscript shiny_app/scripts/fetch_all_seasons.R
#
# For trade history (2019+), set environment variables before running:
#   export ESPN_S2="<your espn_s2 cookie>"
#   export SWID="<your swid cookie>"
# See vignettes/espn_authentication.Rmd for how to extract these from your browser.
#
# Required packages:
#   install.packages(c("ffscrapr", "dplyr", "purrr"))

library(ffscrapr)
library(dplyr)
library(purrr)

LEAGUE_ID <- 85601L
ESPN_S2   <- Sys.getenv("ESPN_S2", unset = "")
SWID      <- Sys.getenv("SWID",    unset = "")

if (!nchar(ESPN_S2)) ESPN_S2 <- NULL
if (!nchar(SWID))    SWID    <- NULL

# Resolve shiny_app/data/ relative to this script's location.
# Works both from Rscript CLI and when source()d interactively.
script_dir <- tryCatch(
  dirname(normalizePath(sys.frames()[[1]]$ofile)),
  error = function(e) getwd()
)
DATA_DIR <- normalizePath(file.path(script_dir, "..", "data"), mustWork = FALSE)
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

START_YEAR <- 2012L
END_YEAR   <- as.integer(format(Sys.Date(), "%Y"))

message("=== ESPN Fantasy Baseball Data Fetch ===")
message("League ID : ", LEAGUE_ID)
message("Seasons   : ", START_YEAR, " - ", END_YEAR)
message("Data dir  : ", DATA_DIR)
message("Trade auth: ", if (!is.null(ESPN_S2)) "YES (ESPN_S2 + SWID set)" else "NO (trades will be skipped)")
message("")

# ── Per-season fetch ─────────────────────────────────────────────────────────

fetch_season <- function(season) {
  message("--- Season ", season, " ---")

  conn <- tryCatch(
    espn_connect(
      season    = season,
      league_id = LEAGUE_ID,
      espn_s2   = ESPN_S2,
      swid      = SWID
    ),
    error = function(e) {
      message("  SKIP: espn_connect() failed - ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(conn)) return(invisible(NULL))

  # League metadata
  league <- tryCatch({
    message("  fetching ff_league()...")
    ff_league(conn)
  }, error = function(e) { message("  WARN: ff_league() - ", conditionMessage(e)); NULL })

  # Franchise name / owner mapping for this season
  franchises <- tryCatch({
    message("  fetching ff_franchises()...")
    ff_franchises(conn)
  }, error = function(e) { message("  WARN: ff_franchises() - ", conditionMessage(e)); NULL })

  # Week-by-week matchup results (works back to ~2004)
  schedule <- tryCatch({
    message("  fetching ff_schedule()...")
    ff_schedule(conn)
  }, error = function(e) { message("  WARN: ff_schedule() - ", conditionMessage(e)); NULL })

  # Season standings: W/L/T, PF, PA, final rank
  standings <- tryCatch({
    message("  fetching ff_standings()...")
    ff_standings(conn)
  }, error = function(e) { message("  WARN: ff_standings() - ", conditionMessage(e)); NULL })

  # Weekly hitting + pitching stats (only available 2018+; slow due to per-day API calls)
  weekly_stats <- if (season >= 2018L) {
    tryCatch({
      checkmax <- ffscrapr:::.espn_week_checkmax(conn)
      max_wk   <- checkmax$max_week
      if (max_wk == 0L) {
        message("  SKIP weekly_stats: no completed weeks yet")
        NULL
      } else {
        message("  fetching get_weekly_stats() weeks 1-", max_wk, "...")
        get_weekly_stats(conn, week = seq_len(max_wk))
      }
    }, error = function(e) {
      message("  WARN: get_weekly_stats() - ", conditionMessage(e)); NULL
    })
  } else {
    message("  SKIP weekly_stats: not available before 2018")
    NULL
  }

  # Transactions / trades (requires auth cookies, only 2019+)
  transactions <- if (season >= 2019L && !is.null(ESPN_S2)) {
    tryCatch({
      message("  fetching ff_transactions()...")
      txn <- ff_transactions(conn)
      if (!is.null(txn) && nrow(txn) > 0) {
        txn %>% filter(type == "TRADE")
      } else {
        NULL
      }
    }, error = function(e) {
      message("  WARN: ff_transactions() - ", conditionMessage(e)); NULL
    })
  } else {
    if (season >= 2019L) message("  SKIP transactions: ESPN_S2/SWID not set")
    NULL
  }

  out <- list(
    season       = season,
    league       = league,
    franchises   = franchises,
    schedule     = schedule,
    standings    = standings,
    weekly_stats = weekly_stats,
    transactions = transactions
  )

  path <- file.path(DATA_DIR, paste0("season_", season, ".rds"))
  saveRDS(out, path)
  message("  Saved -> ", path)
  invisible(out)
}

# Run all seasons
seasons_fetched <- 0L
seasons_skipped <- 0L

for (yr in START_YEAR:END_YEAR) {
  result <- fetch_season(yr)
  if (is.null(result)) {
    seasons_skipped <- seasons_skipped + 1L
  } else {
    seasons_fetched <- seasons_fetched + 1L
  }
}

message("")
message("=== Done ===")
message("Fetched : ", seasons_fetched, " seasons")
message("Skipped : ", seasons_skipped, " seasons")
message("Files in: ", DATA_DIR)
