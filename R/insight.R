#' Helping function for analysing sim/obs discharge from hydrological models.
#'
#' @param date A date vector.
#' @param sim A numeric vector of simulated time series.
#' @param obs A numeric vector of observed time series.
#' @param na.rm A boolean for na remove.
#' @param interactive A boolean for interactive plot (ggplotly). Only for 1. Daily hydrograph
#'
#' @return A list with 6 items comparing sim and obs:
#' - daily_hydrograph: A plot of daily discharge (can be interactive)
#' - performance_criteria: A list of different performance criteria (see hydros1mple::score)
#' - monthly_hydrograph: A plot of mean monthly discharge
#' - yearly_hydrograph: A plot of yearly discharge
#' - boxplot: A boxplot of the distribution of discharge
#' - cdf: The cumulative distribution function of discharge
#' @export
#' @import scales
#' @import ggplot2
#'
#' @examples
#' data(KarstMod_dataset)
#' insight(KarstMod_dataset$date, KarstMod_dataset$QobsS + runif(365, -2, +10), KarstMod_dataset$QobsS)

insight <- function(date, sim, obs, na.rm = FALSE, interactive = FALSE) {

  library(scales)

  export <- list()

  # 1. Daily hydrograph
  export[["daily_hydrograph"]] <- data.frame(date, sim, obs) |>
    tidyr::pivot_longer(c(sim, obs)) |>
    ggplot(aes(date, value, color = name)) +
    geom_line() +
    scale_color_manual(name = "", values = c("obs" = hydros1mple::cblind_bw_palette[1],
                                             "sim" = hydros1mple::cblind_bw_palette[3])) +
    {if (!interactive) ylab(expression(paste("Discharge [L"^3~T^-1, "]")))} +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom",
          axis.title.x = element_blank())

  if (interactive) export[["daily_hydrograph"]] <- plotly::ggplotly(export[["daily_hydrograph"]]) |>
    plotly::layout(legend = list(orientation = 'h',
                                 x = 0.4,
                                 y = -0.1),
                   yaxis = list(title = "Discharge [m3/s]"))

  # 2. Performance criteria
  export[["performance_criteria"]] <- hydros1mple::score(sim, obs, na.rm = na.rm, format = c("list"))

  # 3. Montly hydrograph
  export[["monthly_hydrograph"]] <- data.frame(date, sim, obs) |>
    tidyr::pivot_longer(c(sim, obs)) |>
    dplyr::group_by(name, month = lubridate::month(date)) |>
    dplyr::summarise(value = mean(value, na.rm = na.rm)) |>
    ggplot(aes(month, value, fill = name)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_continuous(labels = month.abb, breaks = 1:12) +
    scale_fill_manual(name = "", values = c("obs" = hydros1mple::cblind_bw_palette[1],
                                            "sim" = hydros1mple::cblind_bw_palette[3])) +
    ylab(expression(paste("Mean discharge [L"^3~T^-1, "]"))) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom",
          axis.title.x = element_blank())

  # 4. Yearly hydrograph
  export[["yearly_hydrograph"]] <- data.frame(date, sim, obs) |>
    tidyr::pivot_longer(c(sim, obs)) |>
    dplyr::group_by(name, year = lubridate::year(date)) |>
    dplyr::summarise(value = mean(value, na.rm = na.rm)) |>
    ggplot(aes(as.character(year), value, fill = name)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(name = "", values = c("obs" = hydros1mple::cblind_bw_palette[1],
                                            "sim" = hydros1mple::cblind_bw_palette[3])) +
    ylab(expression(paste("Mean discharge [L"^3~T^-1, "]"))) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom",
          axis.title.x = element_blank())

  # 5. Boxplot
  export[["boxplot"]] <- data.frame(sim, obs) |>
    tidyr::pivot_longer(c(sim, obs)) |>
    ggplot(aes(name, value, fill = name)) +
    geom_boxplot() +
    scale_fill_manual(name = "", values = c("obs" = "white",
                                            "sim" = hydros1mple::cblind_bw_palette[3])) +
    ylab(expression(paste("Mean discharge [L"^3~T^-1, "]"))) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())

  # 6. Cumulative distribution function (CDF)
  export[["cdf"]] <- list("obs" = obs,
       "sim" = sim) |>
    purrr::map(~ cdf(.)) |>
    purrr::map(as.data.frame) |>
    purrr::list_rbind(names_to = "name") |>
    tidyr::pivot_wider(names_from = name, values_from = value) |>
    dplyr::arrange(prob_ex) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c("obs", "sim")), ~ zoo::na.approx(., na.rm = na.rm))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c("obs", "sim")), ~ tidyr::replace_na(., 0))) |>
    tidyr::pivot_longer(c(sim, obs)) |>
    ggplot(aes(prob_ex, value, color = name)) +
    geom_line() +
    scale_x_continuous(trans = scales::compose_trans("log10", "reverse")) +
    scale_color_manual(name = "", values = c("obs" = hydros1mple::cblind_bw_palette[1],
                                             "sim" = hydros1mple::cblind_bw_palette[3])) +
    xlab("Probability exceedance") +
    ylab(expression(paste("Discharge [L"^3~T^-1, "]"))) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")

  return(export)

}
