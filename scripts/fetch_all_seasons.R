# fetch_all_seasons.R
#
# Run this script ONCE (offline) to populate shiny_app/data/ with cached .rds files.
# Re-run at the end of each season, or weekly during an active season.
#
# This script lives OUTSIDE shiny_app/ on purpose: it calls library(ffscrapr),
# which the deployed Shiny app never needs (the app only reads pre-fetched
# .rds files). Keeping it out of shiny_app/ ensures deployment tools (rsconnect,
# Posit Connect Cloud) never see or bundle it, so ffscrapr is never mistaken
# for a runtime dependency.
#
# SETUP — install the dev version of ffscrapr from this repo first:
#   devtools::install(".")           # from the repo root in R
#   # OR: install.packages("devtools"); devtools::install_github("hkingsley-code/ffscrapr")
#
# USAGE (from the repo root):
#   Rscript scripts/fetch_all_seasons.R
#
# For trade history (2019+), provide your ESPN login cookies via a GITIGNORED
# `.Renviron` file at the repo root (never commit real cookies). Copy the
# template and fill it in:
#   cp .Renviron.example .Renviron   # then edit .Renviron with your values
# `.Renviron` is auto-loaded below. It should contain:
#   ESPN_S2=<your espn_s2 cookie>
#   SWID={your-swid-including-braces}
# Alternatively export ESPN_S2 / SWID in your shell before running.
# See vignettes/espn_authentication.Rmd for how to extract these from your browser.

library(ffscrapr)
library(dplyr)
library(purrr)

# Load a repo-root .Renviron (gitignored) if present, regardless of cwd.
local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_dir <- if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
  } else tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)),
                  error = function(e) getwd())
  renv <- normalizePath(file.path(script_dir, "..", ".Renviron"), mustWork = FALSE)
  if (file.exists(renv)) readRenviron(renv)
})

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

# Some seasons' week 1 spans an early opening-series stretch (real MLB games, but
# before the league's recognized season start) that ESPN folds into week 1's day
# range. Listed seasons have week 1 recomputed using ONLY these scoring periods
# instead of ESPN's auto-derived (over-inclusive) day list.
# 2026: periods 1-5 = Mar 25-29 (early opener, excluded); 6-12 = Mar 30 onward (kept).
# Verified against ff_schedule()'s official week-1 total (diff 16.0 of 4018.5, ~0.4%).
WEEK1_PERIOD_OVERRIDE <- list(
  "2026" = 6:12
)

# ── Robust DATA_DIR resolution ────────────────────────────────────────────────
# Resolves to <repo_root>/shiny_app/data regardless of current working
# directory. Priority order:
#   1. --file= arg  (Rscript path/to/fetch_all_seasons.R)
#   2. sys.frames   (source("path/to/fetch_all_seasons.R"))
#   3. getwd()      (only correct when run from the repo root)

.resolve_data_dir <- function() {
  # 1. Rscript CLI
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[1])
    script_dir  <- dirname(normalizePath(script_path, mustWork = FALSE))
    return(normalizePath(file.path(script_dir, "..", "shiny_app", "data"), mustWork = FALSE))
  }

  # 2. source()
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frames()[[1]]$ofile)),
    error = function(e) NULL
  )
  if (!is.null(script_dir)) {
    return(normalizePath(file.path(script_dir, "..", "shiny_app", "data"), mustWork = FALSE))
  }

  # 3. Fallback — assumes CWD is the repo root
  message("Note: could not auto-detect script location. Assuming CWD is the repo root.")
  normalizePath(file.path(getwd(), "shiny_app", "data"), mustWork = FALSE)
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

  # Apply the week-1 period override, if this season has one configured.
  season_key <- as.character(season)
  if (!is.null(weekly_stats) && season_key %in% names(WEEK1_PERIOD_OVERRIDE)) {
    periods <- WEEK1_PERIOD_OVERRIDE[[season_key]]
    corrected_wk1 <- tryCatch(
      ffscrapr:::.get_weekly_stats_custom_periods(conn, week = 1L, periods = periods),
      error = function(e) {
        message("  WARN: week-1 override failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(corrected_wk1)) {
      weekly_stats <- weekly_stats %>%
        filter(week != 1L) %>%
        bind_rows(corrected_wk1)
      message("  Week 1 recomputed using scoring periods ", paste(range(periods), collapse = "-"))
    }
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
