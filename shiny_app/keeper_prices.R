# keeper_prices.R — sourced from global.R
#
# Computes next-season keeper prices from this season's auction draft + current
# roster. League rule: keeper_price = max(this season's draft price, $10), UNLESS
# the player was ALREADY a keeper this season, in which case it's
# ceiling(this season's draft price * 1.5) — the draft price for a kept player
# already reflects last year's formula, so this escalates it again.
# Verified against the league's keeper spreadsheet (2026 -> 2027 prices):
#   Aaron Judge      $75 -> $113  (75 * 1.5 = 112.5, ceil -> 113)
#   Shohei Ohtani    $90 -> $135  (90 * 1.5 = 135)
#   Corbin Carroll   $30 -> $45   (30 * 1.5 = 45)
#
# Players on the current roster with no matching draft record this season
# (waiver pickups / in-season adds) default to $10, correctable via the
# `keeper_price_overrides` tibble in corrections.R.

#' Compute next-season keeper prices
#'
#' @param draft_df this season's draft (from ff_draft()): needs player_id,
#'   bid_amount, is_keeper
#' @param roster_df this season's current roster (from ff_rosters()): needs
#'   player_id, player_name, pos, team, franchise_id, franchise_name — this is
#'   the base, so players are attributed to their CURRENT team even if traded
#'   or added off waivers after the draft
#' @param overrides_df manual corrections (season-filtered `keeper_price_overrides`
#'   from corrections.R): needs player_id, override_price
#' @param min_price minimum keeper price for drafted-but-cheap or undrafted players
#' @noRd
compute_keeper_prices <- function(draft_df, roster_df, overrides_df = NULL, min_price = 10) {
  empty_result <- tibble::tibble(
    player_id          = numeric(0),
    player_name        = character(0),
    pos                = character(0),
    team               = character(0),
    franchise_id       = character(0),
    franchise_name     = character(0),
    draft_price        = numeric(0),
    is_keeper          = logical(0),
    next_season_price  = numeric(0),
    price_source       = character(0)
  )

  if (is.null(roster_df) || nrow(roster_df) == 0) {
    return(empty_result)
  }

  draft_slim <- if (!is.null(draft_df) && nrow(draft_df) > 0) {
    draft_df %>%
      dplyr::mutate(player_id = as.numeric(.data$player_id)) %>%
      dplyr::select(dplyr::any_of(c("player_id", "bid_amount", "is_keeper"))) %>%
      dplyr::distinct(.data$player_id, .keep_all = TRUE)
  } else {
    tibble::tibble(player_id = numeric(0), bid_amount = numeric(0), is_keeper = logical(0))
  }

  overrides_slim <- if (!is.null(overrides_df) && nrow(overrides_df) > 0) {
    overrides_df %>%
      dplyr::mutate(player_id = as.numeric(.data$player_id)) %>%
      dplyr::select("player_id", "override_price")
  } else {
    tibble::tibble(player_id = numeric(0), override_price = numeric(0))
  }

  roster_df %>%
    dplyr::mutate(
      player_id    = as.numeric(.data$player_id),
      franchise_id = as.character(.data$franchise_id)
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "player_id", "player_name", "pos", "team", "franchise_id", "franchise_name"
    ))) %>%
    dplyr::distinct(.data$player_id, .data$franchise_id, .keep_all = TRUE) %>%
    dplyr::left_join(draft_slim, by = "player_id") %>%
    dplyr::left_join(overrides_slim, by = "player_id") %>%
    dplyr::mutate(
      is_keeper       = dplyr::coalesce(.data$is_keeper, FALSE),
      escalated_price = ceiling(.data$bid_amount * 1.5),
      base_price      = dplyr::case_when(
        .data$is_keeper & !is.na(.data$bid_amount) ~ .data$escalated_price,
        !is.na(.data$bid_amount)                    ~ pmax(.data$bid_amount, min_price),
        TRUE                                         ~ NA_real_
      ),
      price_source = dplyr::case_when(
        !is.na(.data$override_price)                ~ "manual-override",
        .data$is_keeper & !is.na(.data$bid_amount)   ~ "escalated",
        !is.na(.data$bid_amount)                     ~ "draft-or-min",
        TRUE                                         ~ "waiver-default"
      ),
      next_season_price = dplyr::coalesce(.data$override_price, .data$base_price, as.numeric(min_price))
    ) %>%
    dplyr::rename(draft_price = "bid_amount") %>%
    dplyr::select(
      "player_id", "player_name", "pos", "team", "franchise_id", "franchise_name",
      "draft_price", "is_keeper", "next_season_price", "price_source"
    ) %>%
    dplyr::arrange(.data$franchise_name, dplyr::desc(.data$next_season_price))
}
