#' Convert analyses in mg/L to meq/L.
#'
#' @param data A dataframe with ions as columns.
#' @param ions A string vector of ions to convert.
#'
#' @return A dataframe with a new columns of each ion in meq/L.
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' data(pc_data)
#'
#' mgl_to_meq(pc_data, ions = c("Ca", "Mg", "Na", "K", "SO4", "Cl", "HCO3"))

mgl_to_meq <- function(data, ions) {

  # atomic weights (mg/mmol)
  Ca_w <- 40.078
  Mg_w <- 24.305
  Na_w <- 22.990
  K_w <- 39.098
  Si_w <- 28.085
  Sr_w <- 87.62
  S_w <- 32.06
  N_w <- 14.007
  Br_w <- 79.904
  O_w <- 15.999
  P_w <- 30.973761998
  H_w <- 1.008
  F_w <- 18.998403163
  C_w <- 12.011
  Cl_w <- 35.45

  # absolute value of charge (meq/mmol)
  NH4_ch <- 1
  Ca_ch <- 2
  Mg_ch <- 2
  Na_ch <- 1
  K_ch <- 1
  Si_ch <- 1
  Sr_ch <- 2
  HCO3_ch <- 1
  CO3_ch <- 2
  F_ch <- 1
  Cl_ch <- 1
  Br_ch <- 1
  NO3_ch <- 1
  NO2_ch <- 1
  PO4_ch <- 3
  SO4_ch <- 2

  # molar mass (mg/meq)
  NH4_mm <- NH4_ch / (N_w + 4 * H_w)
  Ca_mm <- Ca_ch / Ca_w
  Mg_mm <- Mg_ch / Mg_w
  Na_mm <- Na_ch / Na_w
  K_mm <- K_ch / K_w
  Si_mm <- Si_ch / Si_w
  Sr_mm <- Sr_ch / Sr_w
  HCO3_mm <- HCO3_ch / (H_w + C_w + 3 * O_w)
  F_mm <- F_ch / F_w
  Cl_mm <- Cl_ch / Cl_w
  Br_mm <- Br_ch / Br_w
  NO3_mm <- NO3_ch / (N_w + 3 * O_w)
  NO2_mm <- NO2_ch / (N_w + 2 * O_w)
  PO4_mm <- PO4_ch / (P_w + 4 * O_w)
  SO4_mm <- SO4_ch / (S_w + 4 * O_w)
  CO3_mm <- CO3_ch / (C_w + 3 * O_w)

  # concentration in meq/L
  for (ion in ions) {
    ion_meq <- paste0(ion, "_meq")
    ion_mm <- paste0(ion, "_mm")

    data <- data %>%
      mutate({{ ion_meq }} := .data[[ion]] * !! sym(ion_mm))
  }

  return(data)
}
