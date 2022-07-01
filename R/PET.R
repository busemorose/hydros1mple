#' Calculation of Potential Evapotranspiration (PET) with different methods.
#'
#' @param date A date vector.
#' @param t A numeric vector.
#' @param method A string value. Either "oudin2005", "hargreaves1985" or "turc1961".
#' @param latitude A numeric value. Latitude (in degrees) of the meteorological station. This is for estimating Ra.
#' @param krs A numeric value. Radiation adjustment coefficient (generally between 0.16 and 0.19).
#' @param tmax A numeric vector.
#' @param tmin A numeric vector.
#' @param rh A numeric vector.
#' @param Rs A numeric vector.
#' @param k1 A numeric value. Used for Oudin's method.
#' @param k2 A numeric value. Used for Oudin's method.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' data(KarstMod_dataset)
#' PET(date = KarstMod_dataset$date, t = KarstMod_dataset$T, latitude = 48)

PET <- function(date, t, method = c("oudin2005", "hargreaves1985", "turc1961"), tmax = NULL, tmin = NULL, rh = NULL, latitude = NULL, krs = 0.17, Rs = NULL, k1 = 100, k2 = 5) {

  # Calculate extraterrestrial radiation

  if (is.null(latitude))
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
    oudin <- ifelse((t + 5) > 0, (Ra / le) * ((t + k2) / k1), 0)
    return(oudin)
  }

  if (method == "hargreaves1985") {
    if (!all(lengths(list(t, tmin, tmax)) == length(date)))
      stop("date, t, tmin and tmax must be specified and with the same length.")
    hargreaves <- 0.0135 * krs * Ra * 0.408 * sqrt(mean(tmax) - mean(tmin)) * (t + 17.8)
    return(hargreaves)
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
    turc <- ifelse(rh >= 50,
                   0.013 * (t / (t + 15)) * (Rs * 23.9 + 50),
                   0.013 * (t / (t + 15)) * (Rs * 23.9 + 50) * (1 + (50 - rh) / 70))
    turc <- ifelse(turc < 0, 0, turc)
    return(turc)
  }
}

