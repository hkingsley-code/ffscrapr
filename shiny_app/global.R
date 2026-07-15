# global.R — loaded once at Shiny startup

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(purrr)
})

# Hand-maintained corrections (champions, playoff weeks, name aliases)
source("corrections.R", local = FALSE)

# Playoff/relegation Monte Carlo simulation + keeper price computation
source("simulate_playoffs.R", local = FALSE)
source("keeper_prices.R", local = FALSE)

DATA_DIR <- file.path(getwd(), "data")

# ── Load and combine all season .rds files ───────────────────────────────────

rds_files <- list.files(DATA_DIR, pattern = "^season_\\d{4}\\.rds$", full.names = TRUE)

if (length(rds_files) == 0) {
  stop(
    "No season data found in ", DATA_DIR, ".\n",
    "Run scripts/fetch_all_seasons.R first."
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
all_draft        <- bind_season_element(season_list, "draft")
all_rosters      <- bind_season_element(season_list, "rosters")

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

if (nrow(all_draft) > 0) {
  all_draft <- mutate(all_draft, franchise_id = as.character(franchise_id))
}
if (nrow(all_rosters) > 0) {
  all_rosters <- mutate(all_rosters, franchise_id = as.character(franchise_id))
}

# ── Owner identity ───────────────────────────────────────────────────────────
# franchise_id is a TEAM SLOT, reused by different people and changed by the
# same person across seasons. The stable identity is the owner (user_name).
# `owner_key` normalises whitespace and applies name_aliases so one human is one
# key — this is what all cross-season aggregation groups by.

canon_owner <- function(x) {
  x <- str_squish(x)
  hit <- !is.na(x) & x %in% names(name_aliases)
  x[hit] <- unname(name_aliases[x[hit]])
  x
}

# Per (season, franchise_id) → owner identity for that season.
# `orig_owner` is ESPN's account owner; `owner_key` applies owner_overrides.
owner_lookup <- all_franchises %>%
  transmute(
    season,
    franchise_id,
    orig_owner = canon_owner(coalesce(user_name, franchise_name)),
    team_name  = franchise_name
  ) %>%
  left_join(
    owner_overrides %>%
      transmute(season = as.integer(season),
                franchise_id = as.character(franchise_id),
                override = canon_owner(owner)),
    by = c("season", "franchise_id")
  ) %>%
  mutate(
    owner_key     = coalesce(override, orig_owner),
    owner_display = owner_key
  ) %>%
  select(season, franchise_id, owner_key, orig_owner, team_name, owner_display)

# ── Recompute regular-season records for manual-playoff seasons ───────────────
# For seasons in `playoff_start_week`, ESPN's stored standings are unreliable
# (they mix in the manually-run first playoff week). Rebuild those seasons'
# standings from the schedule using only weeks BEFORE the playoff start.

.recompute_standings <- function(sched, season_franchises) {
  # sched: one season's schedule rows with a completed result
  base <- sched %>%
    filter(!is.na(result)) %>%
    group_by(franchise_id) %>%
    summarise(
      h2h_wins       = sum(result == "W"),
      h2h_losses     = sum(result == "L"),
      h2h_ties       = sum(result == "T"),
      points_for     = sum(franchise_score,  na.rm = TRUE),
      points_against = sum(opponent_score,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      h2h_winpct = (h2h_wins + 0.5 * h2h_ties) /
        pmax(h2h_wins + h2h_losses + h2h_ties, 1)
    )

  # All-play: within each week, how many teams you would have beaten
  allplay <- sched %>%
    filter(!is.na(result)) %>%
    group_by(week) %>%
    mutate(
      allplay_wins   = rank(franchise_score) - 1,
      allplay_losses = n() - 1 - allplay_wins
    ) %>%
    ungroup() %>%
    group_by(franchise_id) %>%
    summarise(
      allplay_wins   = sum(allplay_wins,   na.rm = TRUE),
      allplay_losses = sum(allplay_losses, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      allplay_winpct = round(allplay_wins / pmax(allplay_wins + allplay_losses, 1), 3)
    )

  base %>%
    left_join(allplay, by = "franchise_id") %>%
    # league_rank = regular-season rank (wins, then points_for)
    arrange(desc(h2h_wins + 0.5 * h2h_ties), desc(points_for)) %>%
    mutate(league_rank = row_number()) %>%
    left_join(season_franchises, by = "franchise_id") %>%
    select(
      franchise_id, franchise_name, league_rank,
      h2h_wins, h2h_losses, h2h_ties, h2h_winpct,
      points_for, points_against,
      allplay_wins, allplay_losses, allplay_winpct
    )
}

if (nrow(playoff_start_week) > 0) {
  for (i in seq_len(nrow(playoff_start_week))) {
    yr  <- playoff_start_week$season[i]
    cut <- playoff_start_week$week[i]

    sched_yr <- all_schedules %>% filter(season == yr, week < cut)
    if (nrow(sched_yr) == 0) next

    fr_yr <- all_franchises %>%
      filter(season == yr) %>%
      distinct(franchise_id, franchise_name)

    recomputed <- .recompute_standings(sched_yr, fr_yr) %>%
      mutate(season = yr)

    # Splice: drop ESPN's rows for this season, insert recomputed
    all_standings <- all_standings %>%
      filter(season != yr) %>%
      bind_rows(recomputed)
  }
}

# ── Attach owner_key to schedules & standings ────────────────────────────────

all_standings <- all_standings %>%
  left_join(owner_lookup %>% select(season, franchise_id, owner_key),
            by = c("season", "franchise_id"))

all_schedules <- all_schedules %>%
  left_join(owner_lookup %>% select(season, franchise_id, owner_key),
            by = c("season", "franchise_id")) %>%
  left_join(owner_lookup %>% select(season, franchise_id,
                                    opponent_owner_key = owner_key),
            by = c("season", "opponent_id" = "franchise_id"))

# ── Mask in-progress (partial) weeks ─────────────────────────────────────────
# ESPN reports a premature W/L for the current, partially-played week (its total
# scoring is a fraction of a finished week). Detect weeks whose scoring is far
# below the season's completed-week norm and blank their result, so an in-progress
# matchup never counts as a win or loss (fixes H2H showing current games as losses).
week_completeness <- all_schedules %>%
  filter(!is.na(result)) %>%
  group_by(season, week) %>%
  summarise(wk_total = sum(abs(franchise_score), na.rm = TRUE), .groups = "drop") %>%
  group_by(season) %>%
  mutate(incomplete = wk_total < 0.4 * median(wk_total)) %>%
  ungroup()

all_schedules <- all_schedules %>%
  left_join(week_completeness %>% select(season, week, incomplete),
            by = c("season", "week")) %>%
  mutate(result = if_else(coalesce(incomplete, FALSE), NA_character_, result)) %>%
  select(-incomplete)

# Weekly stats carry the owner name (user_name) but no franchise_id, so map to
# owner_key via (season, original owner name) — this also applies owner_overrides.
if (nrow(all_weekly_stats) > 0) {
  weekly_owner_map <- owner_lookup %>%
    distinct(season, orig_owner, owner_key)
  all_weekly_stats <- all_weekly_stats %>%
    mutate(orig_owner = canon_owner(user_name)) %>%
    left_join(weekly_owner_map, by = c("season", "orig_owner")) %>%
    mutate(owner_key = coalesce(owner_key, orig_owner)) %>%
    select(-orig_owner)
}

# ── Head-to-head schedule (playoff weeks excluded for manual seasons) ─────────
# Used by the Head-to-Head tab. For seasons with a manual playoff start, drop
# weeks >= that week so H2H reflects the true regular season.
h2h_schedule <- all_schedules %>%
  left_join(playoff_start_week, by = "season") %>%
  filter(is.na(week.y) | week.x < week.y) %>%
  rename(week = week.x) %>%
  select(-week.y)

# ── Derived: resolve team / owner names for display ──────────────────────────

# Per-season name lookup (used to show names as they were in each season).
# display_name is the (canonicalized, override-aware) owner from owner_lookup so
# a person reads the same everywhere; the season's team name is preserved as-is.
franchise_season_lookup <- all_franchises %>%
  select(season, franchise_id, franchise_name) %>%
  left_join(owner_lookup %>% select(season, franchise_id, display_name = owner_key),
            by = c("season", "franchise_id")) %>%
  mutate(display_name = coalesce(display_name, franchise_name))

# Canonical owner display (used for cross-season labels)
owner_display_lookup <- owner_lookup %>%
  distinct(owner_key, .keep_all = TRUE) %>%
  select(owner_key, owner_display)

resolve_names <- function(df) {
  df %>%
    left_join(
      franchise_season_lookup %>% select(season, franchise_id, display_name),
      by = c("season", "franchise_id")
    )
}

# Add display names to schedules and standings upfront
schedules_named <- all_schedules %>%
  resolve_names() %>%
  left_join(
    franchise_season_lookup %>% select(season, franchise_id, opponent_name = display_name),
    by = c("season", "opponent_id" = "franchise_id")
  )

standings_named <- all_standings %>%
  resolve_names() %>%
  left_join(owner_display_lookup, by = "owner_key")

# ── Champions (authoritative, canonicalized) ─────────────────────────────────
champions_tbl <- champions %>%
  mutate(season = as.integer(season),
         champion = canon_owner(champion))

# ── Available season ranges ──────────────────────────────────────────────────

ALL_SEASONS       <- sort(unique(all_schedules$season))
STATS_SEASONS     <- sort(unique(all_weekly_stats$season))
TXN_SEASONS       <- if (nrow(all_transactions) > 0) sort(unique(all_transactions$season)) else integer(0)
HAS_TRADE_DATA    <- length(TXN_SEASONS) > 0

# All owners (canonical) for Head-to-Head dropdowns
ALL_OWNERS <- sort(unique(owner_lookup$owner_key))

# League name from the most recent season's metadata
LEAGUE_NAME <- tryCatch({
  meta <- season_list[[length(season_list)]]$league
  if (!is.null(meta) && "league_name" %in% names(meta)) meta$league_name else "League History"
}, error = function(e) "League History")

# All unique franchise display names (canonical, by slot) for the Trades filter
franchise_canonical <- all_franchises %>%
  arrange(desc(season)) %>%
  distinct(franchise_id, .keep_all = TRUE) %>%
  select(franchise_id, franchise_name, user_name) %>%
  mutate(display_name = coalesce(user_name, franchise_name))

ALL_TEAMS <- franchise_canonical %>%
  arrange(display_name) %>%
  pull(display_name) %>%
  unique()

# ── Playoff/relegation odds (current season only) ────────────────────────────
# Computed once at app startup, not per-session — a 10,000-trial simulation
# shouldn't re-run for every visitor. Requires division_id/division_name from
# ff_franchises()/ff_standings() (see R/espn_franchises.R's .espn_divisions())
# for the current season's data; if that season's .rds predates the division
# fields (needs a re-run of scripts/update_current_season.R), this degrades to
# NULL and the Playoff Odds tab shows a message instead of erroring the app.
CURRENT_SEASON <- max(ALL_SEASONS)

current_standings <- standings_named %>%
  filter(season == CURRENT_SEASON)

SIM_RESULT <- tryCatch(
  simulate_season(
    schedule_df  = all_schedules %>% filter(season == CURRENT_SEASON),
    standings_df = current_standings,
    n_trials     = 10000,
    seed         = 20260713
  ),
  error = function(e) {
    message("SIM_RESULT unavailable: ", conditionMessage(e))
    NULL
  }
)

# Trial count for the interactive "What If" panel's per-session reactive
# recompute (see server.R). Measured at ~13-17s per call at 10,000 trials —
# unusable for a per-click reactive — vs. ~1.7s at 1,000 trials, which is an
# acceptable responsiveness/precision trade-off for an exploratory tool.
WHATIF_N_TRIALS <- 1000

# ── Keeper prices for next season (current season only) ──────────────────────
# Requires ff_draft()/ff_rosters() to have been fetched for the current season
# (see scripts/update_current_season.R); degrades to an empty table otherwise.
# (all_draft/all_rosters can be a 0-column empty tibble if NO cached season has
# this data yet — filtering by `season` on that would error, so guard on nrow()
# first, mirroring how all_transactions/HAS_TRADE_DATA is guarded above.)
HAS_DRAFT_DATA   <- nrow(all_draft) > 0   && CURRENT_SEASON %in% all_draft$season
HAS_ROSTER_DATA  <- nrow(all_rosters) > 0 && CURRENT_SEASON %in% all_rosters$season

KEEPER_TABLE <- tryCatch(
  compute_keeper_prices(
    draft_df     = if (HAS_DRAFT_DATA)  filter(all_draft,   season == CURRENT_SEASON) else NULL,
    roster_df    = if (HAS_ROSTER_DATA) filter(all_rosters, season == CURRENT_SEASON) else NULL,
    overrides_df = filter(keeper_price_overrides, season == CURRENT_SEASON)
  ),
  error = function(e) {
    message("KEEPER_TABLE unavailable: ", conditionMessage(e))
    compute_keeper_prices(NULL, NULL, NULL)
  }
)
