# update_current_season.R
#
# Daily maintenance script: re-fetch ONLY the current season from ESPN, refresh
# shiny_app/data/season_<year>.rds, and redeploy the app to Posit Connect Cloud.
# Historical seasons are frozen and left untouched.
#
# Designed to be run unattended on a schedule (Windows Task Scheduler / cron).
#
# PREREQUISITES (one-time):
#   * Dev ffscrapr installed:            devtools::install(".")
#   * App already deployed once:         rsconnect::deployApp("shiny_app", appName = "league-85601")
#   * ESPN cookies in a gitignored .Renviron at the repo root (see .Renviron.example).
#     Cookies expire every few weeks — when the app stops updating, paste fresh
#     ESPN_S2 / SWID into .Renviron.
#
# RUN (from the repo root):
#   Rscript scripts/update_current_season.R

suppressPackageStartupMessages({
  library(ffscrapr)
  library(dplyr)
  library(purrr)
})

if (!exists("get_weekly_stats", mode = "function")) {
  stop("get_weekly_stats() not found — install the dev ffscrapr: devtools::install('.')")
}

LEAGUE_ID <- 85601L
APP_NAME  <- "league-85601"
SEASON    <- as.integer(format(Sys.Date(), "%Y"))

# Some seasons' week 1 spans an early opening-series stretch (real MLB games, but
# before the league's recognized season start) that ESPN folds into week 1's day
# range. Listed seasons have week 1 recomputed using ONLY these scoring periods
# instead of ESPN's auto-derived (over-inclusive) day list.
# 2026: periods 1-5 = Mar 25-29 (early opener, excluded); 6-12 = Mar 30 onward (kept).
# Verified against ff_schedule()'s official week-1 total (diff 16.0 of 4018.5, ~0.4%).
WEEK1_PERIOD_OVERRIDE <- list(
  "2026" = 6:12
)

# ── Resolve repo root from this script's location (robust to cwd) ─────────────
.script_dir <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
  } else tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)),
                  error = function(e) getwd())
})
REPO_ROOT <- normalizePath(file.path(.script_dir, ".."), mustWork = FALSE)
APP_DIR   <- file.path(REPO_ROOT, "shiny_app")
DATA_DIR  <- file.path(APP_DIR, "data")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Load ESPN cookies from the gitignored repo-root .Renviron ────────────────
renv <- file.path(REPO_ROOT, ".Renviron")
if (file.exists(renv)) readRenviron(renv)
ESPN_S2 <- Sys.getenv("ESPN_S2", unset = "")
SWID    <- Sys.getenv("SWID",    unset = "")
if (!nchar(ESPN_S2)) ESPN_S2 <- NULL
if (!nchar(SWID))    SWID    <- NULL

log <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

log("=== Current-season update: ", SEASON, " (league ", LEAGUE_ID, ") ===")
if (is.null(ESPN_S2)) {
  log("WARNING: no ESPN_S2/SWID in .Renviron — the current season needs cookies; aborting.")
  quit(status = 1)
}

# ── Fetch the current season ─────────────────────────────────────────────────
conn <- espn_connect(season = SEASON, league_id = LEAGUE_ID,
                     espn_s2 = ESPN_S2, swid = SWID)

league <- tryCatch(ff_league(conn), error = function(e) {
  log("ff_league() failed: ", conditionMessage(e)); NULL
})
if (is.null(league)) {
  log("No league data for ", SEASON, " (season may not have started, or cookies expired). Aborting.")
  quit(status = 1)
}

franchises <- tryCatch(ff_franchises(conn),  error = function(e) { log("ff_franchises: ", conditionMessage(e)); NULL })
schedule   <- tryCatch(ff_schedule(conn),    error = function(e) { log("ff_schedule: ",   conditionMessage(e)); NULL })
standings  <- tryCatch(ff_standings(conn),   error = function(e) { log("ff_standings: ",  conditionMessage(e)); NULL })

weekly_stats <- tryCatch({
  checkmax <- ffscrapr:::.espn_week_checkmax(conn)
  if (checkmax$max_week == 0L) {
    log("No completed weeks yet."); NULL
  } else {
    log("Fetching weekly stats weeks 1-", checkmax$max_week, " (slow)...")
    get_weekly_stats(conn, week = seq_len(checkmax$max_week))
  }
}, error = function(e) { log("get_weekly_stats: ", conditionMessage(e)); NULL })

# Apply the week-1 period override, if this season has one configured.
season_key <- as.character(SEASON)
if (!is.null(weekly_stats) && season_key %in% names(WEEK1_PERIOD_OVERRIDE)) {
  periods <- WEEK1_PERIOD_OVERRIDE[[season_key]]
  corrected_wk1 <- tryCatch(
    ffscrapr:::.get_weekly_stats_custom_periods(conn, week = 1L, periods = periods),
    error = function(e) { log("week-1 override failed: ", conditionMessage(e)); NULL }
  )
  if (!is.null(corrected_wk1)) {
    weekly_stats <- weekly_stats %>%
      filter(week != 1L) %>%
      bind_rows(corrected_wk1)
    log("Week 1 recomputed using scoring periods ", paste(range(periods), collapse = "-"))
  }
}

transactions <- tryCatch({
  txn <- ff_transactions(conn)
  if (!is.null(txn) && nrow(txn) > 0) filter(txn, type == "TRADE") else NULL
}, error = function(e) { log("ff_transactions: ", conditionMessage(e)); NULL })

draft   <- tryCatch(ff_draft(conn),   error = function(e) { log("ff_draft: ",   conditionMessage(e)); NULL })
rosters <- tryCatch(ff_rosters(conn), error = function(e) { log("ff_rosters: ", conditionMessage(e)); NULL })

# Guard: don't overwrite good data with an empty/broken pull
if (is.null(schedule) || nrow(schedule) == 0) {
  log("Schedule came back empty — refusing to overwrite existing data. Aborting.")
  quit(status = 1)
}

out <- list(
  season = SEASON, league = league, franchises = franchises,
  schedule = schedule, standings = standings,
  weekly_stats = weekly_stats, transactions = transactions,
  draft = draft, rosters = rosters
)
path <- file.path(DATA_DIR, paste0("season_", SEASON, ".rds"))
saveRDS(out, path)
log("Saved -> ", path)

# ── Redeploy ─────────────────────────────────────────────────────────────────
# rsconnect.httr2 = FALSE works around a known rsconnect/httr2 deploy bug.
log("Deploying to Posit Connect Cloud...")
options(rsconnect.httr2 = FALSE)
tryCatch({
  rsconnect::deployApp(appDir = APP_DIR, appName = APP_NAME, forceUpdate = TRUE)
  log("Deploy complete.")
}, error = function(e) {
  log("Deploy failed: ", conditionMessage(e))
  quit(status = 1)
})

log("=== Done ===")
