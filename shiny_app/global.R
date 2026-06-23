# global.R — loaded once at Shiny startup

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(purrr)
})

DATA_DIR <- file.path(getwd(), "data")

# ── Load and combine all season .rds files ───────────────────────────────────

rds_files <- list.files(DATA_DIR, pattern = "^season_\\d{4}\\.rds$", full.names = TRUE)

if (length(rds_files) == 0) {
  stop(
    "No season data found in ", DATA_DIR, ".\n",
    "Run shiny_app/scripts/fetch_all_seasons.R first."
  )
}

season_list <- map(rds_files, readRDS)

# Helper: bind a named element across seasons, tagging each row with season
bind_season_element <- function(lst, elem) {
  map_dfr(lst, function(s) {
    df <- s[[elem]]
    if (is.null(df) || nrow(df) == 0) return(NULL)
    mutate(df, season = as.integer(s$season))
  })
}

all_franchises   <- bind_season_element(season_list, "franchises")
all_schedules    <- bind_season_element(season_list, "schedule")
all_standings    <- bind_season_element(season_list, "standings")
all_weekly_stats <- bind_season_element(season_list, "weekly_stats")
all_transactions <- bind_season_element(season_list, "transactions")

# Normalise franchise_id to character for consistent joining
all_franchises   <- mutate(all_franchises,   franchise_id = as.character(franchise_id))
all_schedules    <- mutate(all_schedules,    franchise_id = as.character(franchise_id),
                                             opponent_id  = as.character(opponent_id))
all_standings    <- mutate(all_standings,    franchise_id = as.character(franchise_id))

if (nrow(all_transactions) > 0) {
  all_transactions <- mutate(all_transactions,
                             franchise_id  = as.character(franchise_id),
                             trade_partner = as.character(trade_partner))
}

# Canonical name per franchise_id: most recent season's name
franchise_canonical <- all_franchises %>%
  arrange(desc(season)) %>%
  distinct(franchise_id, .keep_all = TRUE) %>%
  select(franchise_id, franchise_name, user_name) %>%
  mutate(display_name = coalesce(user_name, franchise_name))

# ── Derived: resolve team names in schedule & standings ──────────────────────

# Per-season name lookup (used to show names as they were in each season)
franchise_season_lookup <- all_franchises %>%
  select(season, franchise_id,
         franchise_name,
         display_name = user_name) %>%
  mutate(display_name = coalesce(display_name, franchise_name))

resolve_names <- function(df, by_season = TRUE) {
  if (by_season) {
    df %>%
      left_join(
        franchise_season_lookup %>% select(season, franchise_id, display_name),
        by = c("season", "franchise_id")
      )
  } else {
    df %>%
      left_join(
        franchise_canonical %>% select(franchise_id, display_name),
        by = "franchise_id"
      )
  }
}

# Add display names to schedules and standings upfront
schedules_named <- all_schedules %>%
  resolve_names() %>%
  left_join(
    franchise_season_lookup %>% select(season, franchise_id, opponent_name = display_name),
    by = c("season", "opponent_id" = "franchise_id")
  )

standings_named <- all_standings %>%
  resolve_names()

# ── Available season ranges ──────────────────────────────────────────────────

ALL_SEASONS       <- sort(unique(all_schedules$season))
STATS_SEASONS     <- sort(unique(all_weekly_stats$season))
TXN_SEASONS       <- if (nrow(all_transactions) > 0) sort(unique(all_transactions$season)) else integer(0)
HAS_TRADE_DATA    <- length(TXN_SEASONS) > 0

# League name from the most recent season's metadata
LEAGUE_NAME <- tryCatch({
  meta <- season_list[[length(season_list)]]$league
  if (!is.null(meta) && "league_name" %in% names(meta)) meta$league_name else "League History"
}, error = function(e) "League History")

# All unique franchise display names (canonical) for filter dropdowns
ALL_TEAMS <- franchise_canonical %>%
  arrange(display_name) %>%
  pull(display_name) %>%
  unique()
