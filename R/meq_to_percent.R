#' Convert analyses in meq/L to percent for Piper diagram.
#'
#' @param data A dataframe with ions as columns.
#' @param cations A string vector of cations.
#' @param anions A string vector of anions.
#'
#' @return A dataframe with a new columns of each ion in percent.
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' data(pc_data)
#'
#' # First convert to meq/L
#' pc_data_meq <- mgl_to_meq(pc_data, ions = c("Ca", "Mg", "Na", "K", "SO4", "Cl", "HCO3"))
#'
#' # Define anions argument to exclude "CO3" which is absent in this dataset
#' meq_to_percent(pc_data, anions = c("Cl", "SO4", "HCO3"))

meq_to_percent <- function (data,
                            cations = c("Ca", "Mg", "Na", "K"),
                            anions = c("Cl", "SO4", "HCO3", "CO3")) {

  # Calculate total cations and anions
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_cation = sum(!!!syms(paste0(cations, "_meq"))),
      total_anion = sum(!!!syms(paste0(anions, "_meq")))
    )

  # Concentration in percent
  ions = c(cations, anions)

  for (ion in ions) {
    type <- dplyr::case_when(ion %in% cations ~ "total_cation",
                             ion %in% anions ~ "total_anion",
                             TRUE ~ "error")

    ion_meq <- paste0(ion, "_meq")
    ion_meq_p <- paste0(ion, "_meq_p")

    data <- data %>%
      dplyr::mutate({{ ion_meq_p }} := 100 * (.data[[ion_meq]] / .data[[type]]))
  }

  return(data)
}
