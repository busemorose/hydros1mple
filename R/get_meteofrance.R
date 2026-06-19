#' Download daily precipitation data from Météo-France API
#'
#' @description
#' Downloads daily precipitation data from the Météo-France public API
#' (\code{meteo-api.data.gouv.fr}) for a set of stations, iterating backwards
#' in time by chunks until no data is returned or a request fails.
#'
#' @param station_ids Integer vector. Météo-France station IDs to query.
#' @param dpt Integer. Department number used by the API endpoint. Default: \code{84}.
#' @param window Integer. Number of years per API request chunk. Default: \code{5}.
#' @param base_url Character. Base URL of the API. Default: \code{"https://meteo-api.data.gouv.fr/api/clim/base_quot_vent/"}.
#'
#' @return A \code{data.frame} with columns \code{name} (station name), \code{date}, and \code{P} (precipitation in mm).
#'
#' @examples
#' \dontrun{
#' data <- get_meteofrance(
#'   station_ids = c(84015002, 84107002, 84085004),
#'   dpt = 84
#' )
#' }
#'
#' @importFrom httr GET status_code content
#' @importFrom lubridate year
#' @importFrom dplyr bind_rows mutate select arrange
#'
#' @export
get_meteofrance <- function(station_ids,
                            dpt = 84,
                            window = 5,
                            base_url = "https://meteo-api.data.gouv.fr/api/clim/base_quot_vent/") {

  all_data <- list()
  current_max <- lubridate::year(Sys.Date())

  repeat {
    current_min <- current_max - window + 1

    url <- paste0(base_url, dpt, "/csv/?",
                  "num_postes=", paste(station_ids, collapse = ","),
                  "&anneemin=", current_min,
                  "&anneemax=", current_max)

    response <- httr::GET(url)

    if (httr::status_code(response) != 200) {
      message("Request failed with status ", httr::status_code(response), " — stopping.")
      break
    }

    chunk <- httr::content(response, as = "parsed", type = "text/csv")

    if (is.null(chunk) || nrow(chunk) == 0) {
      message("No data for ", current_min, "-", current_max, " — stopping.")
      break
    }

    message("Downloaded ", nrow(chunk), " rows for ", current_min, "-", current_max)
    all_data[[length(all_data) + 1]] <- chunk

    current_max <- current_min - 1
  }

  dplyr::bind_rows(all_data) |>
    dplyr::mutate(
      date = as.Date(as.character(aaaammjj), format = "%Y%m%d")
    ) |>
    dplyr::arrange(date) |>
    dplyr::select(date, dplyr::everything())
}
