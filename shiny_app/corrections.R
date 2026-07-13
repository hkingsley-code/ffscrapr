# corrections.R — hand-maintained overrides, sourced at the top of global.R.
#
# This is the ONE place to correct league history that ESPN's API gets wrong
# (manual playoff structures, missing seasons, owner name variants). Everything
# here is plain data; global.R applies it.

suppressPackageStartupMessages({
  library(tibble)
})

# ── Owner name aliases ───────────────────────────────────────────────────────
# Fold spelling variants onto one canonical owner label. Applied everywhere an
# owner identity is derived, so the champions list below and the data agree.
# (str_squish is also applied automatically, so whitespace variants need no entry.)
name_aliases <- c(
  "Nicholas Gardner" = "Nick Gardner"
)

# ── Authoritative champions ──────────────────────────────────────────────────
# Single source of truth for BOTH the Championship History page and the all-time
# title counts, so they can never disagree. Champion names must match the
# canonicalized owner label (str_squish + name_aliases above).
champions <- tibble::tribble(
  ~season, ~champion,
  2013L,   "Michael Debolt",   # manual — no 2013 data file exists
  2014L,   "Brent Troop",
  2015L,   "Brent Troop",
  2016L,   "Nick Gardner",     # ESPN wrongly ranks Michael Debolt #1 (manual playoffs)
  2017L,   "Harris Kingsley",
  2018L,   "Matt Cummings",
  2019L,   "Brent Troop",
  2020L,   "Michael Debolt",
  2021L,   "Peyton Lurk",
  2022L,   "Harris Kingsley",
  2023L,   "Peyton Lurk",
  2024L,   "Michael Z",
  2025L,   "Michael Z"
  # 2026 in progress — no champion yet, intentionally omitted
)

# ── Owner overrides ──────────────────────────────────────────────────────────
# When ESPN's account owner for a team slot in a given season isn't who actually
# ran the team. Keyed by (season, franchise_id); the owner name is canonicalized
# and used everywhere that team's records/points/H2H are attributed.
owner_overrides <- tibble::tribble(
  ~season, ~franchise_id, ~owner,
  2018L,   "1",           "Peyton Lurk",   # ESPN shows Harris Kingsley's account
  2019L,   "1",           "Peyton Lurk"
)

# ── Playoff structure overrides ──────────────────────────────────────────────
# Some seasons the league manually made the last "regular season" week the first
# playoff round. Weeks >= `week` here are playoffs and are EXCLUDED when
# recomputing that season's regular-season records from the schedule.
# Only seasons listed here are recomputed; all others keep ESPN's standings.
playoff_start_week <- tibble::tribble(
  ~season, ~week,
  2016L,   20L,
  2017L,   20L
)
