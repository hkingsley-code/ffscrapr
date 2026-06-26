# fetch_all_seasons.R
#
# Run this script ONCE (offline) to populate shiny_app/data/ with cached .rds files.
# Re-run at the end of each season, or weekly during an active season.
#
# SETUP — install the dev version of ffscrapr from this repo first:
#   devtools::install(".")           # from the repo root in R
#   # OR: install.packages("devtools"); devtools::install_github("hkingsley-code/ffscrapr")
#
# USAGE (from the repo root):
#   Rscript shiny_app/scripts/fetch_all_seasons.R
#
# For trade history (2019+), set environment variables before running:
#   Sys.setenv(ESPN_S2 = "<value>", SWID = "<value>")   # in R
#   export ESPN_S2="..." SWID="..."                      # in bash/zsh
# See vignettes/espn_authentication.Rmd for how to extract these from your browser.

library(ffscrapr)
library(dplyr)
library(purrr)

# ── Verify dev version is installed (needed to avoid "parsed not found" bug) ──
if (!exists("get_weekly_stats", mode = "function")) {
  stop(
    "get_weekly_stats() not found — you are likely running the CRAN version of ffscrapr.\n",
    "Install the dev version from the repo root:\n",
    "  devtools::install('.')"
  )
}

LEAGUE_ID <- 85601L
ESPN_S2   <- Sys.getenv("ESPN_S2", unset = "")
SWID      <- Sys.getenv("SWID",    unset = "")

if (!nchar(ESPN_S2)) ESPN_S2 <- NULL
if (!nchar(SWID))    SWID    <- NULL

# ── Robust DATA_DIR resolution ────────────────────────────────────────────────
# Priority order:
#   1. --file= arg  (Rscript path/to/fetch_all_seasons.R)
#   2. sys.frames   (source("path/to/fetch_all_seasons.R"))
#   3. getwd()      (only correct when run from shiny_app/)

.resolve_data_dir <- function() {
  # 1. Rscript CLI
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[1])
    script_dir  <- dirname(normalizePath(script_path, mustWork = FALSE))
    return(normalizePath(file.path(script_dir, "..", "data"), mustWork = FALSE))
  }

  # 2. source()
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frames()[[1]]$ofile)),
    error = function(e) NULL
  )
  if (!is.null(script_dir)) {
    return(normalizePath(file.path(script_dir, "..", "data"), mustWork = FALSE))
  }

  # 3. Fallback — assumes CWD is shiny_app/
  message("Note: could not auto-detect script location. Placing data/ inside your working directory.")
  normalizePath(file.path(getwd(), "data"), mustWork = FALSE)
}

DATA_DIR <- .resolve_data_dir()
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Auto-detect start year from a known-good recent season ────────────────────
# Using ff_league() for 2024 to read the "previousSeasons" field ESPN provides.
.detect_start_year <- function() {
  tryCatch({
    probe_conn <- espn_connect(season = 2024, league_id = LEAGUE_ID,
                               espn_s2 = ESPN_S2, swid = SWID)
    league_meta <- ff_league(probe_conn)
    start_raw <- strsplit(league_meta$years_active, "-")[[1]][1]
    yr <- suppressWarnings(as.integer(start_raw))
    if (!is.na(yr) && yr >= 2002L && yr <= 2024L) yr else 2015L
  }, error = function(e) {
    message("Could not auto-detect league start year; defaulting to 2015.")
    2015L
  })
}

message("=== ESPN Fantasy Baseball Data Fetch ===")
message("League ID : ", LEAGUE_ID)
message("Data dir  : ", DATA_DIR)
message("Trade auth: ", if (!is.null(ESPN_S2)) "YES" else "NO (set ESPN_S2 + SWID to unlock 2019+ trades)")
message("")
message("Detecting league start year...")
START_YEAR <- .detect_start_year()
END_YEAR   <- as.integer(format(Sys.Date(), "%Y"))
message("Seasons   : ", START_YEAR, " - ", END_YEAR)
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
      message("  SKIP: espn_connect() failed — ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(conn)) return(invisible(NULL))

  # League metadata
  league <- tryCatch({
    message("  fetching ff_league()...")
    ff_league(conn)
  }, error = function(e) {
    message("  WARN: ff_league() — ", conditionMessage(e))
    NULL
  })

  # If ff_league() returned NULL, the season likely doesn't exist for this league
  if (is.null(league)) {
    message("  SKIP: league data unavailable for ", season, " (season may not exist for this league)")
    return(invisible(NULL))
  }

  # Franchise name / owner mapping for this season
  franchises <- tryCatch({
    message("  fetching ff_franchises()...")
    ff_franchises(conn)
  }, error = function(e) {
    message("  WARN: ff_franchises() — ", conditionMessage(e))
    NULL
  })

  # Week-by-week matchup results (works back to ~2004)
  schedule <- tryCatch({
    message("  fetching ff_schedule()...")
    ff_schedule(conn)
  }, error = function(e) {
    message("  WARN: ff_schedule() — ", conditionMessage(e))
    NULL
  })

  # Season standings: W/L/T, PF, PA, final rank
  standings <- tryCatch({
    message("  fetching ff_standings()...")
    ff_standings(conn)
  }, error = function(e) {
    message("  WARN: ff_standings() — ", conditionMessage(e))
    NULL
  })

  # Weekly hitting + pitching stats (only available 2018+; slow — one API call per day per week)
  weekly_stats <- if (season >= 2018L) {
    tryCatch({
      checkmax <- ffscrapr:::.espn_week_checkmax(conn)
      max_wk   <- checkmax$max_week
      if (max_wk == 0L) {
        message("  SKIP weekly_stats: no completed weeks yet")
        NULL
      } else {
        message("  fetching get_weekly_stats() weeks 1-", max_wk, " (this is slow)...")
        get_weekly_stats(conn, week = seq_len(max_wk))
      }
    }, error = function(e) {
      message("  WARN: get_weekly_stats() — ", conditionMessage(e))
      NULL
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
      message("  WARN: ff_transactions() — ", conditionMessage(e))
      NULL
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
  message("  OK -> ", path)
  invisible(out)
}

# ── Run ───────────────────────────────────────────────────────────────────────

n_ok   <- 0L
n_skip <- 0L

for (yr in START_YEAR:END_YEAR) {
  result <- fetch_season(yr)
  if (is.null(result)) n_skip <- n_skip + 1L else n_ok <- n_ok + 1L
}

message("")
message("=== Done ===")
message("Saved  : ", n_ok,   " seasons  →  ", DATA_DIR)
message("Skipped: ", n_skip, " seasons (no data or not yet played)")
