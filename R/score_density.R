#' Generate plot to show the density of criterion score by hydrological year.
#'
#' @param date A date vector.
#' @param sim A numeric vector of simulated time series.
#' @param obs A numeric vector of observed time series.
#' @param interval A numeric value corresponding to the desired class interval.
#' @param freq A string specifying the frequence of analysis, either "hyear" for hydrological year or "month".
#' @param month_hyear A numeric value corresponding to the first month of the hydrological year.
#' @param crit A string value corresponding to the desired performance criterion for the evaluation.
#' @param sf A numeric vector for specifying scaling factors if needed for correlation, variability and bias, respectively. Default values are 1-1-1.
#' @param bar_width A numeric value for specifying the width of the bar.
#' @param palette A two string vector with the desired extreme colors for the palette.
#'
#' @return A plot. Best export dimensions for default width are width = 8, height = 5.
#' @export
#' @import ggplot2
#'
#' @examples
#' data(KarstMod_dataset)
#' score_density(KarstMod_dataset$date, KarstMod_dataset$QobsS + runif(365, -2, +10), KarstMod_dataset$QobsS, interval = 10)

score_density <- function(date, sim, obs, interval, freq = c("hyear", "month"), month_hyear = 9,
                          crit = c("NSE", "KGE", "rpearson", "rspearman", "KGE_m", "KGE_m2", "KGENP", "LME", "LCE"),
                          sf = c(1, 1, 1),
                          bar_width = 0.048,
                          palette = c("#eff3ff", "#2473B6")) {

  # Get output format
  crit <- match.arg(crit)
  freq <- match.arg(freq)

  # Create palette
  pal <- colorRampPalette(colors = palette)

  # Create dataframe
  model <- data.frame(date, sim, obs)

  # Calculate hydrological year and mean annual discharges
  if (freq == "hyear") {
    data <- model |>
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    hyear = dplyr::case_when(month < month_hyear ~ year - 1,
                                             TRUE ~ year)) |>
      dplyr::group_by(!!sym(freq)) |>
      dplyr::summarise(mean = mean(obs, na.rm = TRUE),
                       score = hydros1mple::score(sim, obs, crit = crit, sf = sf))
  } else {
    data <- model |>
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    hyear = dplyr::case_when(month < month_hyear ~ year - 1,
                                             TRUE ~ year)) |>
      dplyr::group_by(!!sym(freq), year) |>
      dplyr::summarise(mean = mean(obs, na.rm = TRUE),
                       score = hydros1mple::score(sim, obs, crit = crit, sf = sf))
  }

  # Define classes for discharges and scores
  class <- data |>
    dplyr::filter(score >= 0) |>
    dplyr::mutate(mean_class = floor(signif(mean, pmax(2, trunc(log10(mean) + 1))) / interval) * interval,
                  score_class = floor(signif(score, pmax(2, trunc(log10(score) + 1))) / 0.05) * 0.05)

  # Get appropriate number of colos
  colors <- pal(length(unique(class$mean_class)))

  # Define appropriate factors
  order <- sort(unique(class$mean_class), decreasing = TRUE)

  # Plot
  tick_offset <- 0.025 # shift so that the bars are offset form the ticks

  class |>
    dplyr::mutate(mean_class = factor(mean_class, order)) |>
    dplyr::group_by(mean_class, score_class) |>
    dplyr::summarise(density = dplyr::n() / nrow(class)) |>
    ggplot(aes(score_class, density, color = mean_class, fill = mean_class)) +
    geom_bar(stat = "identity", width = bar_width) +
    scale_color_manual(name = expression(paste(Q[mean], " [", L^3~T^-1, "]")),
                       values = rev(colors),
                       labels = paste0(order, "–", order + interval)) +
    scale_fill_manual(name = expression(paste(Q[mean], " [", L^3~T^-1, "]")),
                      values = rev(colors),
                      labels = paste0(order, "–", order + interval)) +
    scale_x_continuous(breaks = seq(-1, 1, 0.1) - tick_offset,
                       labels = seq(-1, 1, 0.1),
                       expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    coord_cartesian(xlim = c(ifelse(min(class$score, na.rm = TRUE) >= 0,
                                    -tick_offset,
                                    ifelse(min(class$score, na.rm = TRUE) < -1,
                                           -1 - tick_offset,
                                           min(class$score, na.rm = TRUE) - tick_offset)),
                             1 - tick_offset)) +
    xlab(crit) +
    ylab("Density") +
    {if (freq == "hyear") ggtitle(paste0("Model performance per hydrological year (",
                                         month.name[month_hyear], ")"))} +
    {if (freq == "month") ggtitle("Model performance per month")} +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom",
          panel.grid = element_blank()) +
    guides(color = guide_legend(reverse = TRUE),
           fill = guide_legend(reverse = TRUE))

}
