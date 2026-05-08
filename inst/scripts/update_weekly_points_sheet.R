# update_weekly_points_sheet.R
# Fetches stats from ESPN and writes to two Google Sheets tabs:
#   - 'Weekly Points': appends the latest week's per-team stats (idempotent)
#   - 'Season Totals': overwrites with current season-to-date totals
#
# Columns (both tabs): user_name, hitting_points, pitching_points,
#   total_points, H, AB, HR, RBI, R, SB, OBP, SLG, OPS, IP, ERA, WHIP, K, W, SV
# Weekly Points also includes: week
#
# Dependencies: ffscrapr, googlesheets4, dplyr
# First run opens a browser for Google OAuth; subsequent runs reuse the cached token.

library(ffscrapr)
library(googlesheets4)
library(dplyr)

SEASON          <- 2026
LEAGUE_ID       <- 85601
SHEET_ID        <- "1UjlKMIYlwy2qkkOvji_npKxh2AqmmwZCkc7Pjg1dNXs"
WEEKLY_TAB      <- "Weekly Points"
SEASON_TAB      <- "Season Totals"

conn <- espn_connect(season = SEASON, league_id = LEAGUE_ID)

current_week <- espn_current_week(conn)
message("Latest completed week: ", current_week)

new_weekly <- get_weekly_stats(conn, week = current_week)
new_season <- get_weekly_stats(conn, season_total = TRUE)

if (is.null(new_weekly) || nrow(new_weekly) == 0) {
  message("No data available for week ", current_week, ". Nothing written.")
  quit(save = "no", status = 0)
}

gs4_auth()

sheet_id  <- as_sheets_id(SHEET_ID)
tab_names <- sheet_names(sheet_id)

# ── Weekly Points tab (append, idempotent) ──────────────────────────────────
if (!WEEKLY_TAB %in% tab_names) {
  sheet_add(sheet_id, sheet = WEEKLY_TAB)
  message("Created tab: ", WEEKLY_TAB)
}

existing <- read_sheet(sheet_id, sheet = WEEKLY_TAB, col_types = paste0("i", strrep("-", 19)))

if (nrow(existing) > 0 && current_week %in% existing$week) {
  message("Week ", current_week, " already in Weekly Points. No rows appended.")
} else {
  sheet_append(sheet_id, new_weekly, sheet = WEEKLY_TAB)
  message("Appended ", nrow(new_weekly), " rows for week ", current_week, " to Weekly Points.")
}

# ── Season Totals tab (overwrite each run) ──────────────────────────────────
if (!SEASON_TAB %in% tab_names) {
  sheet_add(sheet_id, sheet = SEASON_TAB)
  message("Created tab: ", SEASON_TAB)
}

sheet_write(new_season, ss = sheet_id, sheet = SEASON_TAB)
message("Overwrote Season Totals with data through week ", current_week, ".")
