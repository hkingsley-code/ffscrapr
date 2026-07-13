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

# 3) Per-period STARTER-ONLY point totals — same filter get_weekly_stats() uses
#    (lineup_slot in hitter/pitcher slots, excludes BE/IL), so these numbers are
#    directly comparable to the official matchup total in section E.
.hitter_slots  <- c("C","1B","2B","3B","SS","OF","2B/SS","1B/3B","LF","CF","RF","DH","UTIL","IF")
.pitcher_slots <- c("SP","RP","P")
slot_map <- ffscrapr:::.espn_lineupslot_map()

cat("D) Per scoring period: STARTER-ONLY points (matches get_weekly_stats' own filter):\n")
per <- purrr::map_dfr(sort(gws_days), function(dy) {
  r <- tryCatch(ffscrapr:::.espn_day_roster(dy, WEEK, conn), error = function(e) NULL)
  if (is.null(r) || nrow(r) == 0) return(tibble::tibble(scoring_period = dy, starter_points = NA_real_))
  r <- dplyr::mutate(r, lineup_slot = slot_map[as.character(lineup_id)])
  pts <- r |> dplyr::filter(lineup_slot %in% c(.hitter_slots, .pitcher_slots)) |>
    dplyr::pull(player_score) |> sum(na.rm = TRUE)
  tibble::tibble(scoring_period = dy, starter_points = round(pts, 1))
})
print(as.data.frame(per), row.names = FALSE)

# 4) Official week-1 matchup total for reference (this is what standings/H2H use)
off_total <- tryCatch({
  ff_schedule(conn) |> dplyr::filter(week == WEEK, !is.na(result)) |>
    dplyr::summarise(total = round(sum(franchise_score), 1)) |> dplyr::pull(total)
}, error = function(e) NA)
cat("\nE) Official week-1 matchup total, ALL teams combined (from ff_schedule):", off_total, "\n")

# 5) Cumulative sum from each possible starting period through the end — whichever
#    starting period's cumulative total is closest to E is the true boundary.
cat("\nF) Cumulative STARTER-ONLY total if week 1 were redefined to start at period X:\n")
cum <- purrr::map_dfr(sort(gws_days), function(start_dy) {
  total <- per |> dplyr::filter(scoring_period >= start_dy) |>
    dplyr::pull(starter_points) |> sum(na.rm = TRUE)
  tibble::tibble(start_period = start_dy, cumulative_total = round(total, 1),
                diff_from_official = round(total - off_total, 1))
})
print(as.data.frame(cum), row.names = FALSE)
cat("\n=== The start_period with diff_from_official closest to 0 is the true week-1 start. ===\n")

# 6) PER-TEAM breakdown at the chosen boundary (periods 6:12) vs each team's own
#    official week-1 score — pinpoints whether any residual concentrates on one team.
BOUNDARY <- 6L
cat("\nG) Per-team STARTER-ONLY total for periods >=", BOUNDARY,
    "vs each team's own official week-1 score:\n")
fr <- ff_franchises(conn) |>
  dplyr::mutate(franchise_id = as.character(franchise_id)) |>
  dplyr::select(franchise_id, user_name, franchise_name)

per_team_recompute <- purrr::map_dfr(sort(gws_days[gws_days >= BOUNDARY]), function(dy) {
  r <- tryCatch(ffscrapr:::.espn_day_roster(dy, WEEK, conn), error = function(e) NULL)
  if (is.null(r) || nrow(r) == 0) return(tibble::tibble())
  r <- dplyr::mutate(r,
    franchise_id = as.character(franchise_id),
    lineup_slot  = slot_map[as.character(lineup_id)]
  )
  r |> dplyr::filter(lineup_slot %in% c(.hitter_slots, .pitcher_slots)) |>
    dplyr::select(franchise_id, player_score)
}) |>
  dplyr::group_by(franchise_id) |>
  dplyr::summarise(recomputed = round(sum(player_score, na.rm = TRUE), 1), .groups = "drop")

official_per_team <- ff_schedule(conn) |>
  dplyr::filter(week == WEEK, !is.na(result)) |>
  dplyr::mutate(franchise_id = as.character(franchise_id)) |>
  dplyr::select(franchise_id, official = franchise_score)

g_tbl <- fr |>
  dplyr::left_join(per_team_recompute, by = "franchise_id") |>
  dplyr::left_join(official_per_team,  by = "franchise_id") |>
  dplyr::mutate(diff = round(recomputed - official, 1)) |>
  dplyr::arrange(dplyr::desc(abs(diff)))
print(as.data.frame(g_tbl[, c("user_name", "recomputed", "official", "diff")]), row.names = FALSE)

cat("\n=== Paste sections A-G back. ===\n")
