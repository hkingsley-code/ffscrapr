#### ff_franchises (MFL) ####

#' Get a dataframe of franchise information
#'
#'
#' Returns a tidy dataframe of franchises and any relevant information
#'
#' @param conn a conn object created by \code{ff_connect()}
#'
#' @examples
#' ssb_conn <- ff_connect(platform = "mfl",league_id = 54040,season = 2020)
#' ff_franchises(ssb_conn)
#'
#' @rdname ff_franchises
#' @export

ff_franchises.mfl_conn <- function(conn){

  league <- mfl_getendpoint(conn, "league") %>%
    purrr::pluck("content","league")

  franchises <- league %>%
    purrr::pluck("franchises","franchise") %>%
    tibble::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::rename("franchise_name" = .data$name,
                  "franchise_id" = .data$id) %>%
    dplyr::select('franchise_id','franchise_name',dplyr::everything())

  if(!is.null(league$divisions)){

    divisions <- purrr::pluck(league,"divisions","division") %>%
      tibble::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename("division_name" = .data$name,
                    "division_id" = .data$id)

    franchises <- franchises %>%
      dplyr::left_join(divisions, by = c('division'="division_id"))

  }

  return(franchises)
}