#' Snow routine with P and T data
#'
#' @param precip A numeric vector for precipitation
#' @param temp A numeric vector for temperature
#' @param srad A numeric vector for clear-sky solar radiation
#' @param param A numeric vector for model parameters: 1. Ts = threshold temperature [C]; 2. MF = melt factor [mm/C]; 3. CFR = refreezing factor [-]; 4. CWHv= Water holding capacity of snow [-]
#'
#' @return A list of the output of the snow routine, with P (redistributed precipitation), v (solid volume), vl (liquid volume) and snow (snow)
#' @export

snow_routine <- function(temp, prec, srad, param) {

  #---------------------
  # Preparing parameters
  #---------------------

  Ts <- param[1] # threshold temperature (?C)
  MF <- param[2] # melt factor (mm/C)
  CFR <- param[3] # refreezing factor (-)
  CWH <- param[4] # water holding capacity of snow (-)
  RC <- param[5] # radiation coefficient (-)

  N <- 1:length(prec) # length of the time series

  #----------------
  # Running routine
  #----------------

  P <- rep(0, length(N)) # water leaving the routine/recharge to the soil (mm/timestep)
  rain <- ifelse(temp < Ts, 0, prec) # (mm/timestep)
  snow <- ifelse(temp >= Ts, 0, prec) # (mm/timestep)
  Ta <- ifelse(temp < Ts, 0, temp - Ts) # active temperature for snowmelt
  Tn <- ifelse(temp >= Ts, 0, Ts - temp) # active temperature for refreezing
  m <- rep(0, length(N)) # snowmelt (mm/timestep)
  rfz <- rep(0, length(N)) # refreezing (mm/timestep)
  v <- rep(0, length(N) + 1) # snowpack depth (mm): solid component
  vl <- rep(0, length(N) + 1) # snowpack depth (mm): liquid component

  for (t in rep(N)) {

    m[t] <- min((MF * Ta[t] + RC * srad[t] * Ta[t]), v[t])
    rfz[t] <- min(CFR * MF * Tn[t], vl[t])

    # snowpack dynamics: solid component

    v[t + 1] <- v[t] - m[t] + snow[t] + rfz[t]

    # snowpack dynamics: liquid component

    vl[t + 1] <- vl[t] + m[t] + rain[t] - rfz[t]

    if (vl[t + 1] > CWH * v[t + 1]) { # if the liquid component exceed the snowpack

      # holding capacity

      P[t] <- vl[t + 1] - CWH * v[t + 1]
      vl[t + 1] <- CWH * v[t + 1]

    } else {

      P[t] = 0

    }
  }

  output_snow_routine <- list("P" = P, "v" = v, "vl" = vl, "snow" = snow)
  return(output_snow_routine)

}
