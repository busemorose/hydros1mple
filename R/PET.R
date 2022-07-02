#' Calculation of daily Potential Evapotranspiration (PET) with different methods.
#'
#' This function is a wrapper for several methods of estimation of daily PET. The arguments can be specific to one method and are often set to a default value found in the literature.
#'
#' - \insertCite{oudinWhichPotentialEvapotranspiration2005;textual}{hydros1mple}
#' - \insertCite{hargreavesReferenceCropEvapotranspiration1985;textual}{hydros1mple}
#' - \insertCite{turcEvaluationBesoinsEau1961;textual}{hydros1mple}
#' - \insertCite{thornthwaiteApproachRationalClassification1948;textual}{hydros1mple} adapted by \insertCite{pereiraAdaptationThornthwaiteScheme2004;textual}{hydros1mple}
#' - \insertCite{haudeZurPraktischenBestimmung1954;textual}{hydros1mple}
#'
#'  \insertCite{xiangSimilarityDifferencePotential2020;textual}{hydros1mple} proposed a review of PET methods with a lot of details and references (which can help to understand the equation).
#'
#' @param date A date vector.
#' @param t A numeric vector of mean daily temperature.
#' @param method A string value. Either "oudin2005", "hargreaves1985" or "turc1961".
#' @param latitude A numeric value. Latitude (in degrees) of the meteorological station. This is for estimating Ra.
#' @param krs A numeric value. Radiation adjustment coefficient (generally between 0.16 and 0.19).
#' @param tmax A numeric vector of maximum daily temperature.
#' @param tmin A numeric vector of minimum daily temperature.
#' @param rh A numeric vector of mean daily relative humidity.
#' @param Rs A numeric vector of mean daily solar radiation.
#' @param k1 A numeric value. Used for Oudin's method.
#' @param k2 A numeric value. Used for Oudin's method.
#' @param f_haude A numeric vector of one factor for each month (12 in total). Used for Haude's method.
#' @param k A numeric value corresponding to the calibration coefficient of the effective temperature. Used for Tornthwaite-Pereira's method (they recommended 0.69).
#'
#' @return A numeric vector of mean daily potentation evapotranspiration.
#' @export
#'
#' @references{
#'   \insertAllCited{}
#' }
#'
#' @examples
#' data(KarstMod_dataset)
#' PET(date = KarstMod_dataset$date, t = KarstMod_dataset$T, latitude = 48)

PET <- function(date, t, method = c("oudin2005", "hargreaves_samani1985", "turc1961", "haude1954", "tornthwaite_pereira2004"), tmax = NULL, tmin = NULL, rh = NULL, latitude = NULL, krs = 0.17, Rs = NULL, k1 = 100, k2 = 5, f_haude = c(0.26, 0.26, 0.33, 0.39, 0.39, 0.37, 0.35, 0.33, 0.31, 0.26, 0.26, 0.26), k = 0.69) {

  # Calculate extraterrestrial radiation

  if (is.null(latitude) & !(method %in% c("haude1954")))
    stop("Latitude must be specified.")

  latrad <- (latitude * pi) / 180 # conversion in rad
  yday <- as.numeric(strftime(date, "%j")) # get day of year
  Gsc <- 0.0820 # solar constant in MJ.m-2.min-1
  dr <- 1 + 0.033 * cos((2 * pi * yday) / 365) # inverse relative Earth-Sun distance
  d <- 0.409 * sin((2 * pi * yday) / 365 - 1.39) # solar declinaison (rad)
  ws <- acos(-tan(latrad) * tan(d)) # sunset angle (rad)
  N <- (24 / pi) * ws # daylight hours
  Ra <- ((24 * 60) / pi) * Gsc * dr * (ws * sin(latrad) * sin(d) + cos(latrad) * cos(d) * sin(ws))

  # Calculate PET using the appropriate method

  method <- match.arg(method)

  if (method == "oudin2005") {
    if (!all(lengths(list(t)) == length(date)))
      stop("date and t must be specified and with the same length.")

    le <- 2.501 - 0.002361 * t # latent heat of vaporization in MJ.kg-1
    pet <- ifelse((t + 5) > 0, (Ra / le) * ((t + k2) / k1), 0)
  }

  if (method == "hargreaves_samani1985") {
    if (!all(lengths(list(t, tmin, tmax)) == length(date)))
      stop("date, t, tmin and tmax must be specified and with the same length.")
    # FAO: The corresponding equivalent evaporation in mm day-1 is obtained by
    # multiplying Ra by 0.408 (Equation 20).
    pet <- 0.0023 * Ra * 0.408 * sqrt(tmax - tmin) * (t + 17.8)
  }

  if (method == "turc1961") {
    if (!all(lengths(list(t, rh)) == length(date)))
      stop("date, t, and rh must be specified and with the same length.")
    if (!any(all(lengths(list(tmin, tmax)) == length(date)),
             length(Rs) == length(date)))
      stop("Either Rs or tmin/tmax must be specified and with the same length.")
    if (is.null(Rs)) { # use estimated Rs if no measured Rs is provided
      Rs <- (krs * sqrt(tmax - tmin)) * Ra # estimation of incoming solar radiation Rs
      message("Rs was estimated using the FAO guide for PET")
    }

    pet <- ifelse(rh >= 50,
                  0.013 * (t / (t + 15)) * (Rs * 23.9 + 50),
                  0.013 * (t / (t + 15)) * (Rs * 23.9 + 50) * (1 + (50 - rh) / 70))
  }

  if (method == "haude1954") {
    if (!all(lengths(list(t, rh)) == length(date)))
      stop("date, t, and rh must be specified and with the same length.")

    haude <- data.frame(date, t, rh) |>
      dplyr::mutate(month = lubridate::month(date),
                    f = f_haude[month],
                    PET = f * 6.11 * 10 ^ ((7.48 * t) / (237 + t)) * (1 - rh / 100))
    pet <- haude$PET
  }

  if (method == "tornthwaite_pereira2004") {
    if (!all(lengths(list(tmin, tmax, t)) == length(date)))
      stop("date, t, and rh must be specified and with the same length.")

    tornthwaite <- data.frame(date, t, tmin, tmax) |>
      dplyr::group_by(month = lubridate::month(date)) |>
      dplyr::summarise(tk = mean(t)) |>
      dplyr::mutate(i = dplyr::case_when(tk > 0 ~ (tk / 5) ^ 1.51,
                                         TRUE ~ 0))

    I <- sum(tornthwaite$i)
    a <- (67.5e-8 * I ^ 3) - (77.1e-6 * I ^ 2) + (0.0179 * I) + 0.492
    kd <- N / 360
    tef <- 0.5 * k * (3 * tmax - tmin)
    pet <- ifelse(tef > 26,
                  kd * (-415.85 + 32.24 * tef - 0.43 * tef ^2),
                  ifelse(tef > 0,
                         16 * ((10 * tef) / I) ^ a * kd,
                         0))
  }

  pet <- ifelse(pet < 0, 0, pet)
  return(pet)
}

