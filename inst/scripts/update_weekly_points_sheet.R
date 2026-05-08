# update_weekly_points_sheet.R
# Fetches the latest completed week's fantasy points and stats from ESPN and
# appends them to the 'Weekly Points' tab in Google Sheets.
#
# Columns written: week, user_name, hitting_points, pitching_points,
#   total_points, H, AB, HR, RBI, R, SB, OBP, SLG, OPS, IP, ERA, WHIP, K, W, SV
#
# Dependencies: ffscrapr, googlesheets4, dplyr
# First run opens a browser for Google OAuth; subsequent runs reuse the cached token.

library(ffscrapr)
library(googlesheets4)
library(dplyr)

SEASON    <- 2026
LEAGUE_ID <- 85601
SHEET_ID  <- "1UjlKMIYlwy2qkkOvji_npKxh2AqmmwZCkc7Pjg1dNXs"
SHEET_TAB <- "Weekly Points"

conn <- espn_connect(season = SEASON, league_id = LEAGUE_ID)

current_week <- espn_current_week(conn)
message("Latest completed week: ", current_week)

new_data <- get_weekly_stats(conn, week = current_week)

if (is.null(new_data) || nrow(new_data) == 0) {
  message("No data available for week ", current_week, ". Nothing written.")
  quit(save = "no", status = 0)
}

gs4_auth()

sheet_id <- as_sheets_id(SHEET_ID)
tab_names <- sheet_names(sheet_id)

if (!SHEET_TAB %in% tab_names) {
  sheet_add(sheet_id, sheet = SHEET_TAB)
  message("Created tab: ", SHEET_TAB)
}

existing <- read_sheet(sheet_id, sheet = SHEET_TAB, col_types = paste0("i", strrep("-", 19)))

if (nrow(existing) > 0 && current_week %in% existing$week) {
  message("Week ", current_week, " already in sheet. No rows appended.")
} else {
  sheet_append(sheet_id, new_data, sheet = SHEET_TAB)
  message("Appended ", nrow(new_data), " rows for week ", current_week, ".")
}
