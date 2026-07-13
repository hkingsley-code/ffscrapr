#### ff_franchises (espn) ####

#' Get a dataframe of franchise information
#'
#' @param conn a conn object created by `ff_connect()`
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'
#'   conn <- espn_connect(season = 2024, league_id = 85601)
#'
#'   ff_franchises(conn)
#' }) # end try
#' }
#'
#' @describeIn ff_franchises ESPN: returns franchise and division information.
#' @export
ff_franchises.espn_conn <- function(conn) {
  # Try mTeam first, fall back to mRoster (mTeam is not supported by all ESPN game types)
  team_endpoint <- espn_getendpoint(conn, view = "mTeam") %>%
    purrr::pluck("content")

  teams_raw <- purrr::pluck(team_endpoint, "teams")

  if (is.null(teams_raw) || length(teams_raw) == 0) {
    team_endpoint <- espn_getendpoint(conn, view = "mRoster") %>%
      purrr::pluck("content")
    teams_raw <- purrr::pluck(team_endpoint, "teams")
  }

  if (is.null(teams_raw) || length(teams_raw) == 0) {
    warning(
      glue::glue(
        "ESPN could not retrieve team data for league {conn$league_id}. ",
        "If this is a private league, supply espn_s2 and swid to espn_connect()."
      ),
      call. = FALSE
    )
    return(tibble::tibble(
      franchise_id   = integer(0),
      franchise_name = character(0)
    ))
  }

  # Members section (owner info) requires auth for private leagues - handle gracefully
  members_raw <- purrr::pluck(team_endpoint, "members")

  if (!is.null(members_raw) && length(members_raw) > 0) {
    members <- members_raw %>%
      purrr::map_dfr(function(m) {
        tibble::tibble(
          user_nickname = m[["displayName"]] %||% m[["name"]] %||% NA_character_,
          user_name     = paste(m[["firstName"]] %||% "", m[["lastName"]] %||% "") %>% trimws(),
          user_id       = m[["id"]] %||% NA_character_
        )
      })
  } else {
    members <- tibble::tibble(
      user_nickname = character(0),
      user_name     = character(0),
      user_id       = character(0)
    )
  }

  teams <- teams_raw %>%
    tibble::tibble() %>%
    tidyr::hoist(
      .col = 1,
      "franchise_id"       = "id",
      "franchise_name"     = "name",
      "franchise_abbrev"   = "abbrev",
      "franchise_location" = "location",
      "franchise_nickname" = "nickname",
      "logo"               = "logo",
      "waiver_order"       = "waiverRank",
      "user_id"            = "primaryOwner",
      "division_id"        = "divisionId"
    ) %>%
    dplyr::left_join(members, by = "user_id") %>%
    dplyr::mutate(division_id = as.character(.data$division_id)) %>%
    dplyr::left_join(.espn_divisions(conn), by = "division_id") %>%
    dplyr::mutate(
      franchise_name = dplyr::coalesce(
        .data$franchise_name,
        trimws(paste(.data$franchise_location, .data$franchise_nickname))
      )
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "franchise_id",
      "franchise_name",
      "franchise_abbrev",
      "logo",
      "waiver_order",
      "user_id",
      "user_name",
      "user_nickname",
      "division_id",
      "division_name"
    )))

  return(teams)
}

#' Get division id/name lookup for a league (ESPN)
#'
#' Divisions live under the `mSettings` view, separately from the `mTeam` view
#' that carries each team's `divisionId` - so this is fetched as its own call
#' and left-joined onto franchises, rather than requesting both views at once
#' (the ESPN API can behave inconsistently when views are combined).
#'
#' @param conn a conn object created by `ff_connect()`
#' @noRd
.espn_divisions <- function(conn) {
  empty <- tibble::tibble(
    division_id   = character(0),
    division_name = character(0)
  )

  settings_endpoint <- tryCatch(
    espn_getendpoint(conn, view = "mSettings") %>% purrr::pluck("content"),
    error = function(e) NULL
  )

  divisions_raw <- purrr::pluck(settings_endpoint, "settings", "scheduleSettings", "divisions")

  if (is.null(divisions_raw) || length(divisions_raw) == 0) {
    return(empty)
  }

  divisions_raw %>%
    tibble::tibble() %>%
    tidyr::hoist(
      .col = 1,
      "division_id"   = "id",
      "division_name" = "name"
    ) %>%
    dplyr::mutate(division_id = as.character(.data$division_id)) %>%
    dplyr::select("division_id", "division_name")
}
