## ff_schedule (ESPN) ##

#' Get a dataframe detailing every game for every franchise
#'
#' @param conn a conn object created by `ff_connect()`
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   espn_conn <- espn_connect(season = 2020, league_id = 899513)
#'   ff_schedule(espn_conn)
#' }) # end try
#' }
#'
#' @describeIn ff_schedule ESPN: returns schedule data, one row for every franchise for every week. Completed games have result data.
#'
#' @export
ff_schedule.espn_conn <- function(conn, ...) {
  matchup_endpoint <-
    espn_getendpoint(
      conn = conn,
      view = "mMatchup"
    )

  schedule <-
    matchup_endpoint %>%
    purrr::pluck("content") %>%
    purrr::pluck("schedule")

  .pluck_team <- function(x) {
    schedule %>%
      purrr::map(~ purrr::pluck(.x, x))
  }
  # .pluck_team_score <- function(x) {
  #    x %>% purrr::map(~purrr::pluck(.x, "cumulativeScore"))
  # }

  h <- .pluck_team("home")
  a <- .pluck_team("away")
  # h_score <- h %>% .pluck_team_score()
  # a_score <- a %>% .pluck_team_score()

  # ESPN's own "winner" field is HOME/AWAY/TIE once a matchup period is
  # finalized, and UNDECIDED while it's still in progress (or hasn't started —
  # both cases show live/partial or zero totalPoints). Deriving `result` from
  # `winner` (rather than just comparing totalPoints) is required: a matchup
  # period can be mid-week with real, nonzero partial scores on both sides,
  # which a naive score comparison would misread as a final result days
  # before ESPN itself considers the week over.
  scores <-
    tibble::tibble(
      "week" = schedule %>% purrr::map_int(~ purrr::pluck(.x, "matchupPeriodId")),
      "winner" = schedule %>% purrr::map_chr(~ purrr::pluck(.x, "winner", .default = NA_character_)),
      "home_id" = h %>% purrr::map_int(~ purrr::pluck(.x, "teamId", .default = NA_integer_)),
      "away_id" = a %>% purrr::map_int(~ purrr::pluck(.x, "teamId", .default = NA_integer_)),
      "home_points" = h %>% purrr::map_dbl(~ purrr::pluck(.x, "totalPoints", .default = 0)),
      "away_points" = a %>% purrr::map_dbl(~ purrr::pluck(.x, "totalPoints", .default = 0))
    ) %>%
    dplyr::mutate(
      home_result = dplyr::case_when(
        .data$winner == "HOME" ~ "W",
        .data$winner == "AWAY" ~ "L",
        .data$winner == "TIE"  ~ "T",
        .data$winner == "UNDECIDED" ~ NA_character_,
        # winner field missing/unrecognized (e.g. an older API shape) — fall
        # back to the previous score-comparison heuristic rather than error.
        is.na(.data$winner) & .data$home_points == 0 & .data$away_points == 0 ~ NA_character_,
        is.na(.data$winner) & .data$home_points > .data$away_points ~ "W",
        is.na(.data$winner) & .data$home_points < .data$away_points ~ "L",
        is.na(.data$winner) ~ "T",
        TRUE ~ NA_character_
      ),
      away_result = dplyr::case_when(
        .data$home_result == "W" ~ "L",
        .data$home_result == "L" ~ "W",
        .data$home_result == "T" ~ "T",
        TRUE ~ NA_character_
      )
    )
  scores <- scores %>% dplyr::select(-"winner")
  # Relabel columns (not a sequential mutate/transmute) to build the
  # away-team's-perspective mirror row — this must be a pure name swap, not
  # `dplyr::transmute(home_id = away_id, away_id = home_id, ...)`, because
  # transmute evaluates assignments in order and each new `home_id`/`away_id`
  # would immediately overwrite the column later expressions read from,
  # silently producing opponent_id == franchise_id (a team playing itself).
  scores2 <- scores
  names(scores2) <- c("week", "away_id", "home_id", "away_points", "home_points",
                       "away_result", "home_result")
  schedule <-
    dplyr::bind_rows(scores, scores2) %>%
    dplyr::arrange(.data$week, .data$home_id, .data$away_id) %>%
    dplyr::rename(
      "franchise_id" = .data$home_id,
      "opponent_id" = .data$away_id,
      "franchise_score" = .data$home_points,
      "opponent_score" = .data$away_points,
      "result" = .data$home_result
    ) %>%
    dplyr::select(
      .data$week,
      .data$franchise_id,
      .data$franchise_score,
      .data$result,
      .data$opponent_id,
      .data$opponent_score
    ) %>%
    dplyr::filter(!is.na(.data$franchise_id))
  return(schedule)
}
