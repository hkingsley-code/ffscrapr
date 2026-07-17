# simulate_playoffs.R — sourced from global.R
#
# Monte Carlo simulation of the rest of the current season: playoff odds,
# playoff seeding odds (seeds 1-6), and relegation/promotion odds.
#
# League rules (do not change without checking with the league):
#   - 12 teams: 6 "Alpha" division, 6 "Beta" division.
#   - Playoffs: top 3 Alpha + top 2 Beta auto-qualify. 6th ("wildcard") spot is
#     whichever non-qualified team (either division) has the most points_for.
#   - Seeds: 1 = Alpha's #1 (bye), 2 = Beta's #1 (bye). Seeds 3-6 = the other
#     four qualifiers, ordered strictly by season points_for (NOT wins/H2H).
#   - Relegation: bottom 2 Alpha teams BY RECORD swap divisions with top 2 Beta
#     teams BY RECORD for next season.
#   - "By record" tiebreaker (division rank + relegation only — NOT seeding):
#     win% first, then head-to-head record between the tied teams, then total
#     season points as a last resort.
#
# Known simplifications (documented, not bugs):
#   - The head-to-head tiebreaker is only resolved for an isolated two-team tie
#     (the common case). A three-or-more-way win% tie falls back straight to
#     points_for, since a "mini-league among N tied teams" tiebreaker is
#     ambiguous and not specified by the league.
#   - Remaining games are simulated as normal-distributed scores, which never
#     tie exactly — ties are not modeled for unplayed games.

#' Monte Carlo simulate the rest of the season
#'
#' @param schedule_df current season's schedule rows (from all_schedules):
#'   needs week, franchise_id, opponent_id, franchise_score, result
#' @param standings_df current season's standings + division info (from
#'   standings_named, once division_id/division_name flow through from
#'   ff_franchises()): needs franchise_id, franchise_name, display_name,
#'   division_id, division_name, h2h_wins, h2h_losses, h2h_ties, points_for
#' @param n_trials number of Monte Carlo trials
#' @param seed optional RNG seed for reproducibility
#' @param forced_winners character vector of franchise_ids forced to win
#'   their current-week remaining matchup (used for the "what if" UI). A team
#'   appears in at most one remaining current-week matchup, so specifying
#'   winners is sufficient — the named team's opponent is implicitly forced
#'   to lose. A franchise_id not present in any remaining matchup is a no-op.
#' @noRd
simulate_season <- function(schedule_df, standings_df, n_trials = 10000, seed = NULL,
                             forced_winners = character(0)) {
  if (!is.null(seed)) set.seed(seed)

  teams_df <- standings_df %>%
    dplyr::mutate(franchise_id = as.character(.data$franchise_id)) %>%
    dplyr::distinct(.data$franchise_id, .keep_all = TRUE)

  if (!all(c("Alpha", "Beta") %in% unique(teams_df$division_name))) {
    stop(
      "simulate_season(): expected division_name values 'Alpha' and 'Beta' — got: ",
      paste(unique(teams_df$division_name), collapse = ", "),
      ". Check that ff_franchises()/ff_standings() are returning ESPN division ",
      "data correctly for this league/season (see .espn_divisions() in ",
      "R/espn_franchises.R)."
    )
  }
  if (sum(teams_df$division_name == "Alpha") != 6 || sum(teams_df$division_name == "Beta") != 6) {
    warning(
      "simulate_season(): expected 6 Alpha / 6 Beta teams — got ",
      sum(teams_df$division_name == "Alpha"), " Alpha / ",
      sum(teams_df$division_name == "Beta"), " Beta. Proceeding, but playoff/",
      "relegation counts (top 3 / top 2 / bottom 2) may not make sense.",
      call. = FALSE
    )
  }

  team_ids <- teams_df$franchise_id
  n_teams  <- length(team_ids)

  schedule_df <- schedule_df %>%
    dplyr::mutate(
      franchise_id = as.character(.data$franchise_id),
      opponent_id  = as.character(.data$opponent_id)
    )

  completed <- schedule_df %>%
    dplyr::filter(!is.na(.data$franchise_score), !is.na(.data$result))

  # ---- per-team scoring distribution (mean/sd) from completed weeks --------
  team_stats <- completed %>%
    dplyr::group_by(.data$franchise_id) %>%
    dplyr::summarise(
      mean_score = mean(.data$franchise_score, na.rm = TRUE),
      sd_score   = sd(.data$franchise_score, na.rm = TRUE),
      n_weeks    = dplyr::n(),
      .groups    = "drop"
    )

  league_mean <- mean(completed$franchise_score, na.rm = TRUE)
  league_sd   <- sd(completed$franchise_score, na.rm = TRUE)
  if (!is.finite(league_sd) || league_sd <= 0) league_sd <- 1

  team_stats <- tibble::tibble(franchise_id = team_ids) %>%
    dplyr::left_join(team_stats, by = "franchise_id") %>%
    dplyr::mutate(
      n_weeks    = dplyr::coalesce(.data$n_weeks, 0L),
      mean_score = dplyr::coalesce(.data$mean_score, league_mean),
      sd_score   = dplyr::if_else(
        .data$n_weeks >= 3 & !is.na(.data$sd_score) & .data$sd_score > 0,
        .data$sd_score, league_sd
      )
    )

  mean_lookup <- setNames(team_stats$mean_score, team_stats$franchise_id)
  sd_lookup   <- setNames(team_stats$sd_score,   team_stats$franchise_id)

  # ---- already-completed base record (from standings, not re-derived) ------
  base_df <- teams_df %>%
    dplyr::transmute(
      franchise_id  = .data$franchise_id,
      division_name = .data$division_name,
      base_wins     = as.integer(dplyr::coalesce(.data$h2h_wins,   0L)),
      base_losses   = as.integer(dplyr::coalesce(.data$h2h_losses, 0L)),
      base_ties     = as.integer(dplyr::coalesce(.data$h2h_ties,   0L)),
      base_pf       = as.numeric(dplyr::coalesce(.data$points_for, 0))
    ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  # ---- completed head-to-head (winner/loser pair counts), constant ---------
  # `completed` has TWO mirrored rows per actual game (one per team's
  # perspective, from ff_schedule()). Keep only one side (franchise_id <
  # opponent_id) BEFORE counting, so two teams playing each other more than
  # once this season (e.g. a divisional rematch) still tally correctly instead
  # of collapsing to a single result via distinct().
  h2h_base <- completed %>%
    dplyr::filter(.data$result %in% c("W", "L")) %>%
    dplyr::filter(as.numeric(.data$franchise_id) < as.numeric(.data$opponent_id)) %>%
    dplyr::transmute(
      winner = dplyr::if_else(.data$result == "W", .data$franchise_id, .data$opponent_id),
      loser  = dplyr::if_else(.data$result == "W", .data$opponent_id, .data$franchise_id)
    ) %>%
    dplyr::count(.data$winner, .data$loser, name = "n")

  # ---- remaining (unplayed) unique matchups ---------------------------------
  remaining <- schedule_df %>%
    dplyr::filter(is.na(.data$franchise_score) | is.na(.data$result)) %>%
    dplyr::filter(.data$franchise_id %in% team_ids, .data$opponent_id %in% team_ids) %>%
    dplyr::filter(as.numeric(.data$franchise_id) < as.numeric(.data$opponent_id)) %>%
    dplyr::distinct(.data$week, .data$franchise_id, .data$opponent_id) %>%
    dplyr::rename(team_a = "franchise_id", team_b = "opponent_id")

  n_matchups <- nrow(remaining)
  if (n_matchups == 0) n_trials <- 1L  # nothing left to simulate — one deterministic "trial"

  # ---- current (nearest upcoming) week's matchups, for the "what if" UI ----
  current_week_matchups <- if (n_matchups > 0) {
    cw_week <- min(remaining$week)
    remaining %>%
      dplyr::filter(.data$week == cw_week) %>%
      dplyr::left_join(
        teams_df %>% dplyr::transmute(
          team_a = .data$franchise_id,
          team_a_name = dplyr::coalesce(.data$display_name, .data$franchise_name)
        ),
        by = "team_a"
      ) %>%
      dplyr::left_join(
        teams_df %>% dplyr::transmute(
          team_b = .data$franchise_id,
          team_b_name = dplyr::coalesce(.data$display_name, .data$franchise_name)
        ),
        by = "team_b"
      )
  } else {
    tibble::tibble(
      week = integer(0), team_a = character(0), team_b = character(0),
      team_a_name = character(0), team_b_name = character(0)
    )
  }

  # ---- simulate scores for remaining matchups (vectorized) -----------------
  inc_wins_mat   <- matrix(0L, nrow = n_trials, ncol = n_teams, dimnames = list(NULL, team_ids))
  inc_losses_mat <- matrix(0L, nrow = n_trials, ncol = n_teams, dimnames = list(NULL, team_ids))
  inc_pf_mat     <- matrix(0,  nrow = n_trials, ncol = n_teams, dimnames = list(NULL, team_ids))
  winner_mat     <- matrix(NA_character_, nrow = n_trials, ncol = max(n_matchups, 1))
  loser_mat      <- matrix(NA_character_, nrow = n_trials, ncol = max(n_matchups, 1))

  if (n_matchups > 0) {
    mean_a <- unname(mean_lookup[remaining$team_a]); sd_a <- unname(sd_lookup[remaining$team_a])
    mean_b <- unname(mean_lookup[remaining$team_b]); sd_b <- unname(sd_lookup[remaining$team_b])

    score_a <- matrix(
      rnorm(n_trials * n_matchups, mean = rep(mean_a, each = n_trials), sd = rep(sd_a, each = n_trials)),
      nrow = n_trials, ncol = n_matchups
    )
    score_b <- matrix(
      rnorm(n_trials * n_matchups, mean = rep(mean_b, each = n_trials), sd = rep(sd_b, each = n_trials)),
      nrow = n_trials, ncol = n_matchups
    )
    a_wins <- score_a > score_b

    # Force specific matchups' win/loss for the "what if" UI, without
    # touching the underlying score draws — points_for (which still drives
    # wildcard/seed 3-6 selection) stays realistically variable even for a
    # forced-outcome matchup. Scoped to the CURRENT week only: a team can
    # appear in `remaining` once per week for the rest of the season, so
    # matching on team ID alone (without the week filter) would force that
    # team to win every one of their remaining games all season, not just
    # this week's — forced_a/forced_b can never both be TRUE for the same m
    # since a team is on only one side of its current-week matchup.
    if (length(forced_winners) > 0) {
      is_current_week <- remaining$week == min(remaining$week)
      forced_a <- is_current_week & (remaining$team_a %in% forced_winners)
      forced_b <- is_current_week & (remaining$team_b %in% forced_winners)
      a_wins[, forced_a] <- TRUE
      a_wins[, forced_b] <- FALSE
    }

    for (m in seq_len(n_matchups)) {
      a <- remaining$team_a[m]; b <- remaining$team_b[m]
      win_a <- a_wins[, m]

      inc_wins_mat[, a]   <- inc_wins_mat[, a]   + as.integer(win_a)
      inc_wins_mat[, b]   <- inc_wins_mat[, b]   + as.integer(!win_a)
      inc_losses_mat[, a] <- inc_losses_mat[, a] + as.integer(!win_a)
      inc_losses_mat[, b] <- inc_losses_mat[, b] + as.integer(win_a)
      inc_pf_mat[, a]     <- inc_pf_mat[, a]     + score_a[, m]
      inc_pf_mat[, b]     <- inc_pf_mat[, b]     + score_b[, m]

      winner_mat[, m] <- ifelse(win_a, a, b)
      loser_mat[, m]  <- ifelse(win_a, b, a)
    }
  }

  h2h_wins_between <- function(x, y, t) {
    base_xy <- 0L
    hit <- h2h_base$winner == x & h2h_base$loser == y
    if (any(hit)) base_xy <- h2h_base$n[hit]
    sim_xy <- if (n_matchups > 0) sum(winner_mat[t, ] == x & loser_mat[t, ] == y, na.rm = TRUE) else 0L
    base_xy + sim_xy
  }

  # ---- accumulators ----------------------------------------------------------
  made_playoffs <- setNames(integer(n_teams), team_ids)
  seed_counts   <- matrix(0L, nrow = n_teams, ncol = 6,
                           dimnames = list(team_ids, paste0("seed_", 1:6)))
  relegated <- setNames(integer(n_teams), team_ids)
  promoted  <- setNames(integer(n_teams), team_ids)

  # ---- per-trial ranking, playoff/seed, and relegation determination -------
  for (t in seq_len(n_trials)) {
    cur <- base_df
    cur$wins    <- cur$base_wins   + inc_wins_mat[t, cur$franchise_id]
    cur$losses  <- cur$base_losses + inc_losses_mat[t, cur$franchise_id]
    cur$ties    <- cur$base_ties
    cur$pf      <- cur$base_pf     + inc_pf_mat[t, cur$franchise_id]
    cur$games   <- cur$wins + cur$losses + cur$ties
    cur$win_pct <- (cur$wins + 0.5 * cur$ties) / pmax(cur$games, 1)

    cur <- cur[order(-cur$win_pct, -cur$pf), ]
    rownames(cur) <- NULL

    n_cur <- nrow(cur)
    if (n_cur > 1) {
      for (i in seq_len(n_cur - 1)) {
        if (isTRUE(all.equal(cur$win_pct[i], cur$win_pct[i + 1]))) {
          prev_tied <- if (i > 1) isTRUE(all.equal(cur$win_pct[i - 1], cur$win_pct[i])) else FALSE
          next_tied <- if (i + 2 <= n_cur) isTRUE(all.equal(cur$win_pct[i + 1], cur$win_pct[i + 2])) else FALSE
          if (!prev_tied && !next_tied) {
            x <- cur$franchise_id[i]; y <- cur$franchise_id[i + 1]
            wx <- h2h_wins_between(x, y, t)
            wy <- h2h_wins_between(y, x, t)
            if (wy > wx) cur[c(i, i + 1), ] <- cur[c(i + 1, i), ]
          }
        }
      }
    }

    alpha <- cur[cur$division_name == "Alpha", , drop = FALSE]
    beta  <- cur[cur$division_name == "Beta",  , drop = FALSE]
    if (nrow(alpha) < 3 || nrow(beta) < 2) next  # malformed division data — skip trial defensively

    alpha_playoff  <- utils::head(alpha$franchise_id, 3)
    beta_playoff   <- utils::head(beta$franchise_id, 2)
    auto_qualified <- c(alpha_playoff, beta_playoff)

    pool     <- cur[!(cur$franchise_id %in% auto_qualified), , drop = FALSE]
    wildcard <- pool$franchise_id[which.max(pool$pf)]

    playoff_teams <- c(auto_qualified, wildcard)
    made_playoffs[playoff_teams] <- made_playoffs[playoff_teams] + 1L

    seed1 <- alpha$franchise_id[1]
    seed2 <- beta$franchise_id[1]
    seed_36_pool <- setdiff(playoff_teams, c(seed1, seed2))
    seed_36_pf   <- cur$pf[match(seed_36_pool, cur$franchise_id)]
    seed_36_ord  <- seed_36_pool[order(-seed_36_pf)]

    seeds <- c(seed1, seed2, seed_36_ord)
    for (k in seq_along(seeds)) {
      seed_counts[seeds[k], k] <- seed_counts[seeds[k], k] + 1L
    }

    relegated_teams <- utils::tail(alpha$franchise_id, 2)
    promoted_teams  <- utils::head(beta$franchise_id, 2)
    relegated[relegated_teams] <- relegated[relegated_teams] + 1L
    promoted[promoted_teams]   <- promoted[promoted_teams] + 1L
  }

  # ---- aggregate to percentages ---------------------------------------------
  playoff_pct   <- made_playoffs / n_trials
  seed_pct      <- seed_counts   / n_trials
  relegated_pct <- relegated     / n_trials
  promoted_pct  <- promoted      / n_trials

  team_odds <- teams_df %>%
    dplyr::transmute(
      franchise_id,
      team           = dplyr::coalesce(.data$display_name, .data$franchise_name),
      division_name  = .data$division_name,
      current_wins   = as.integer(dplyr::coalesce(.data$h2h_wins,   0L)),
      current_losses = as.integer(dplyr::coalesce(.data$h2h_losses, 0L)),
      current_ties   = as.integer(dplyr::coalesce(.data$h2h_ties,   0L)),
      current_pf     = as.numeric(dplyr::coalesce(.data$points_for, 0)),
      playoff_pct    = unname(playoff_pct[.data$franchise_id]),
      seed_1_pct     = unname(seed_pct[.data$franchise_id, "seed_1"]),
      seed_2_pct     = unname(seed_pct[.data$franchise_id, "seed_2"]),
      seed_3_pct     = unname(seed_pct[.data$franchise_id, "seed_3"]),
      seed_4_pct     = unname(seed_pct[.data$franchise_id, "seed_4"]),
      seed_5_pct     = unname(seed_pct[.data$franchise_id, "seed_5"]),
      seed_6_pct     = unname(seed_pct[.data$franchise_id, "seed_6"]),
      bye_pct        = .data$seed_1_pct + .data$seed_2_pct,
      relegated_pct  = unname(relegated_pct[.data$franchise_id]),
      promoted_pct   = unname(promoted_pct[.data$franchise_id])
    ) %>%
    dplyr::arrange(dplyr::desc(.data$playoff_pct), dplyr::desc(.data$current_wins))

  list(
    n_trials    = n_trials,
    team_odds   = team_odds,
    current_week_matchups = current_week_matchups,
    methodology = paste0(
      n_trials, " Monte Carlo trials. Each remaining matchup draws a score for ",
      "each team from a normal distribution fit to that team's own weekly scores ",
      "this season (teams with fewer than 3 completed weeks borrow the league-wide ",
      "spread instead). Division standings and relegation ties are broken by ",
      "head-to-head record between the tied teams (three-or-more-way ties fall ",
      "back to total season points). Playoff seeds 1-2 always go to each ",
      "division's #1 team (first-round bye); the wildcard and seeds 3-6 are ",
      "determined strictly by total season points."
    )
  )
}
