# diagnose_week1.R
#
# One-time diagnostic to identify the pre-season (pre-Mar-30) scoring periods that
# get_weekly_stats() folds into the current season's week 1. Run it once and paste
# the output back so the exact scoringPeriodIds to exclude can be configured.
#
# RUN (from the repo root):
#   Rscript scripts/diagnose_week1.R
#
# Needs the dev ffscrapr + your ESPN cookies in the gitignored .Renviron.

suppressPackageStartupMessages({
  library(ffscrapr)
  library(dplyr)
  library(purrr)
})

LEAGUE_ID <- 85601L
SEASON    <- as.integer(format(Sys.Date(), "%Y"))
WEEK      <- 1L

# ── repo root + cookies ──────────────────────────────────────────────────────
.script_dir <- local({
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(fa) > 0) dirname(normalizePath(sub("^--file=", "", fa[1]), mustWork = FALSE))
  else tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)), error = function(e) getwd())
})
renv <- file.path(.script_dir, "..", ".Renviron")
if (file.exists(renv)) readRenviron(renv)
ESPN_S2 <- Sys.getenv("ESPN_S2", ""); if (!nchar(ESPN_S2)) ESPN_S2 <- NULL
SWID    <- Sys.getenv("SWID", "");    if (!nchar(SWID))    SWID    <- NULL

conn <- espn_connect(season = SEASON, league_id = LEAGUE_ID, espn_s2 = ESPN_S2, swid = SWID)

cat("=== Week-1 scoring-period diagnostic — season", SEASON, "league", LEAGUE_ID, "===\n\n")

# 1) Which scoring periods get_weekly_stats maps to week 1 (from pointsByScoringPeriod)
checkmax <- ffscrapr:::.espn_week_checkmax(conn)
gws_days <- checkmax$matchup_days[[as.character(WEEK)]]
cat("A) Scoring periods get_weekly_stats() uses for week 1 (matchup_days):\n   ",
    paste(sort(gws_days), collapse = ", "), "\n\n")

# 2) ESPN's OFFICIAL scoring periods for matchup period 1 (from settings)
url <- glue::glue(
  "https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/",
  "{SEASON}/segments/0/leagues/{LEAGUE_ID}?view=mSettings"
)
settings <- ffscrapr:::espn_getendpoint_raw(conn, url) |>
  purrr::pluck("content", "settings", "scheduleSettings", "matchupPeriods")
off_days <- settings[[as.character(WEEK)]]
cat("B) ESPN's official matchupPeriods[['1']] (scoring periods ESPN assigns to matchup 1):\n   ",
    if (is.null(off_days)) "<not present>" else paste(sort(unlist(off_days)), collapse = ", "), "\n\n")

if (!is.null(off_days)) {
  extra <- setdiff(sort(gws_days), sort(unlist(off_days)))
  cat("C) Periods in A but NOT in B (candidate pre-season days to drop):\n   ",
      if (length(extra) == 0) "<none — matchupPeriods already matches>" else paste(extra, collapse = ", "),
      "\n\n")
}

# 3) Per-period point totals — sum the PER-PLAYER appliedStatTotal (player_score),
#    which is what get_weekly_stats() actually uses and genuinely varies by day.
#    (franchise_score/totalPoints is the matchup's running CUMULATIVE total and is
#    identical for every day queried — do not use it here, that was the earlier bug.)
cat("D) Per scoring period: real per-day production, summed across all players/teams\n")
cat("   (pre-season days with no MLB games should be ~0; real week-1 days will be large):\n")
per <- purrr::map_dfr(sort(gws_days), function(dy) {
  r <- tryCatch(ffscrapr:::.espn_day_roster(dy, WEEK, conn), error = function(e) NULL)
  pts <- if (is.null(r) || nrow(r) == 0) NA_real_ else sum(r$player_score, na.rm = TRUE)
  n_nonzero <- if (is.null(r) || nrow(r) == 0) NA_integer_ else sum(r$player_score != 0, na.rm = TRUE)
  tibble::tibble(scoring_period = dy, total_player_points = round(pts, 1),
                nonzero_player_rows = n_nonzero)
})
print(as.data.frame(per), row.names = FALSE)

# 4) Official week-1 matchup total for reference
off_total <- tryCatch({
  ff_schedule(conn) |> dplyr::filter(week == WEEK, !is.na(result)) |>
    dplyr::summarise(mean = round(mean(franchise_score), 1)) |> dplyr::pull(mean)
}, error = function(e) NA)
cat("\nE) Official week-1 matchup score, mean/team (from ff_schedule):", off_total, "\n")
cat("   (get_weekly_stats week-1 mean/team is ~491 — the gap is the pre-season days.)\n")

cat("\n=== Paste sections A–E back so we can set the exact periods to exclude. ===\n")
