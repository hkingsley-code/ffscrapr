#### get_weekly_stats ####

#' Get Hitting and Pitching Stats by Team for a Given Week
#'
#' Returns a tidy summary of fantasy points and raw statistics per team for
#' the specified week(s). Hitting rate stats (OBP, SLG, OPS) and pitching
#' rate stats (ERA, WHIP, IP) are computed from accumulated counting stats
#' across all started players, so they reflect true team-week totals rather
#' than simple averages of individual values.
#'
#' Requires an ESPN connection. Falls back to points-only output if the API
#' does not return raw stat categories for a given week.
#'
#' @param conn A connection object created by \code{\link{espn_connect}}.
#' @param week Integer (or integer vector). The scoring week(s) to retrieve.
#'
#' @return A tibble with one row per team per week and columns:
#'   \describe{
#'     \item{week}{Scoring week number.}
#'     \item{user_name}{Owner name (falls back to \code{franchise_name} when
#'       owner info is unavailable).}
#'     \item{hitting_points}{Total fantasy points from hitting lineup slots.}
#'     \item{pitching_points}{Total fantasy points from pitching lineup slots.}
#'     \item{total_points}{Sum of hitting and pitching points.}
#'     \item{H}{Hits.}
#'     \item{AB}{At bats.}
#'     \item{HR}{Home runs.}
#'     \item{RBI}{Runs batted in.}
#'     \item{R}{Runs scored.}
#'     \item{SB}{Stolen bases.}
#'     \item{OBP}{On-base percentage, computed from H + BB + HBP over AB + BB + HBP + SF.}
#'     \item{SLG}{Slugging percentage, computed from TB / AB.}
#'     \item{OPS}{On-base plus slugging.}
#'     \item{IP}{Innings pitched (outs / 3).}
#'     \item{ERA}{Earned run average, computed from ER * 9 / IP.}
#'     \item{WHIP}{Walks plus hits per inning pitched, computed from (H + BB) / IP.}
#'     \item{K}{Pitcher strikeouts.}
#'     \item{W}{Wins.}
#'     \item{SV}{Saves.}
#'   }
#'
#' @examples
#' \donttest{
#' try({
#'   conn <- espn_connect(season = 2026, league_id = 85601)
#'   get_weekly_stats(conn, week = 1)
#' })
#' }
#'
#' @export
get_weekly_stats <- function(conn, week) {
  .hitter_slots <- c(
    "C", "1B", "2B", "3B", "SS", "OF",
    "2B/SS", "1B/3B", "LF", "CF", "RF", "DH", "UTIL", "IF"
  )
  .pitcher_slots <- c("SP", "RP", "P")

  franchises <- ff_franchises(conn) %>%
    dplyr::mutate(
      franchise_id = as.character(.data$franchise_id),
      user_name    = dplyr::coalesce(.data$user_name, .data$franchise_name)
    ) %>%
    dplyr::select("franchise_id", "user_name")

  checkmax     <- .espn_week_checkmax(conn)
  max_week     <- checkmax$max_week
  matchup_days <- checkmax$matchup_days

  run_weeks <- week[week <= max_week]

  if (length(run_weeks) == 0) {
    warning(
      glue::glue(
        "ESPN league_id {conn$league_id} does not have stats for ",
        "{conn$season} weeks {paste(min(week), max(week), sep = '-')}."
      ),
      call. = FALSE
    )
    return(NULL)
  }

  hitting_pts <- espn_hitter_points(conn, weeks = run_weeks) %>%
    dplyr::mutate(franchise_id = as.character(.data$franchise_id)) %>%
    dplyr::select("week", "franchise_id", "hitting_points" = "hitter_points")

  pitching_pts <- espn_pitcher_points(conn, weeks = run_weeks) %>%
    dplyr::mutate(franchise_id = as.character(.data$franchise_id)) %>%
    dplyr::select("week", "franchise_id", "pitching_points" = "pitcher_points")

  raw_all <- purrr::map_dfr(
    run_weeks,
    ~ .espn_week_raw_stats(.x, conn, matchup_days)
  )

  points_base <- hitting_pts %>%
    dplyr::left_join(pitching_pts, by = c("week", "franchise_id")) %>%
    dplyr::left_join(franchises,   by = "franchise_id") %>%
    dplyr::mutate(total_points = .data$hitting_points + .data$pitching_points)

  if (is.null(raw_all) || nrow(raw_all) == 0) {
    return(
      points_base %>%
        dplyr::select("week", "user_name", "hitting_points", "pitching_points", "total_points") %>%
        dplyr::arrange(.data$week, dplyr::desc(.data$total_points))
    )
  }

  slot_map <- .espn_lineupslot_map()

  raw_all <- raw_all %>%
    dplyr::mutate(
      franchise_id = as.character(.data$franchise_id),
      lineup_slot  = slot_map[as.character(.data$lineup_id)] %>% unname()
    )

  for (col in c("AB", "H", "HR", "RBI", "R", "SB", "TB", "B_BB", "HBP", "SF")) {
    if (!col %in% names(raw_all)) raw_all[[col]] <- NA_real_
  }
  for (col in c("OUTS", "ER", "P_H", "P_BB", "K", "W", "SV")) {
    if (!col %in% names(raw_all)) raw_all[[col]] <- NA_real_
  }

  hitting_stats <- raw_all %>%
    dplyr::filter(.data$lineup_slot %in% .hitter_slots) %>%
    dplyr::group_by(.data$week, .data$franchise_id) %>%
    dplyr::summarise(
      AB   = sum(.data$AB,   na.rm = TRUE),
      H    = sum(.data$H,    na.rm = TRUE),
      HR   = sum(.data$HR,   na.rm = TRUE),
      RBI  = sum(.data$RBI,  na.rm = TRUE),
      R    = sum(.data$R,    na.rm = TRUE),
      SB   = sum(.data$SB,   na.rm = TRUE),
      TB   = sum(.data$TB,   na.rm = TRUE),
      B_BB = sum(.data$B_BB, na.rm = TRUE),
      HBP  = sum(.data$HBP,  na.rm = TRUE),
      SF   = sum(.data$SF,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      OBP = dplyr::if_else(
        .data$AB + .data$B_BB + .data$HBP + .data$SF > 0,
        (.data$H + .data$B_BB + .data$HBP) / (.data$AB + .data$B_BB + .data$HBP + .data$SF),
        NA_real_
      ),
      SLG = dplyr::if_else(.data$AB > 0, .data$TB / .data$AB, NA_real_),
      OPS = .data$OBP + .data$SLG
    ) %>%
    dplyr::select("week", "franchise_id", "H", "AB", "HR", "RBI", "R", "SB", "OBP", "SLG", "OPS")

  pitching_stats <- raw_all %>%
    dplyr::filter(.data$lineup_slot %in% .pitcher_slots) %>%
    dplyr::group_by(.data$week, .data$franchise_id) %>%
    dplyr::summarise(
      OUTS = sum(.data$OUTS, na.rm = TRUE),
      ER   = sum(.data$ER,   na.rm = TRUE),
      P_H  = sum(.data$P_H,  na.rm = TRUE),
      P_BB = sum(.data$P_BB, na.rm = TRUE),
      K    = sum(.data$K,    na.rm = TRUE),
      W    = sum(.data$W,    na.rm = TRUE),
      SV   = sum(.data$SV,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      IP   = .data$OUTS / 3,
      ERA  = dplyr::if_else(.data$IP > 0, .data$ER * 9 / .data$IP, NA_real_),
      WHIP = dplyr::if_else(.data$IP > 0, (.data$P_H + .data$P_BB) / .data$IP, NA_real_)
    ) %>%
    dplyr::select("week", "franchise_id", "IP", "ERA", "WHIP", "K", "W", "SV")

  points_base %>%
    dplyr::left_join(hitting_stats,  by = c("week", "franchise_id")) %>%
    dplyr::left_join(pitching_stats, by = c("week", "franchise_id")) %>%
    dplyr::select(
      "week", "user_name",
      "hitting_points", "pitching_points", "total_points",
      "H", "AB", "HR", "RBI", "R", "SB", "OBP", "SLG", "OPS",
      "IP", "ERA", "WHIP", "K", "W", "SV"
    ) %>%
    dplyr::arrange(.data$week, dplyr::desc(.data$total_points))
}
