#' Generate plot to evaluate a model by hydrological year on different subregimes
#'
#' @param date A date vector.
#' @param sim A numeric vector of simulated time series.
#' @param obs A numeric vector of observed time series.
#' @param month_hyear A numeric value corresponding to the first month of the hydrological year.
#' @param lf_quantile A numeric value corresponding to the low flow threshold (lower than).
#' @param hf_quantile A numeric value corresponding to the high flow threshold (higher than).
#' @param crit A string value corresponding to the desired performance criterion for the evaluation.
#' @param sf A numeric vector for specifying scaling factors if needed for correlation, variability and bias, respectively. Default values are 1-1-1.
#' @param rel_heights A numeric vector for specifying the relative height of each panel.
#'
#' @return A plot. Best export dimensions for default rel_heights are width = 8, height = 8.
#' @export
#' @import ggplot2
#'
#' @examples
#' data(KarstMod_dataset)
#' score_year(KarstMod_dataset$date, KarstMod_dataset$QobsS + runif(365, -2, +10), KarstMod_dataset$QobsS)

score_year <- function(date, sim, obs, month_hyear = 9, lf_quantile = 0.1, hf_quantile = 0.9,
                       crit = c("NSE", "KGE", "rpearson", "rspearman", "KGE_m", "KGE_m2", "KGENP", "LME", "LCE"),
                       sf = c(1, 1, 1),
                       rel_heights = c(1, 0.9, 1.55)) {

  # Plot function
  q_plot <- function(df) {

    ggplot(df, aes(hyear, score, fill = mean)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      #geom_text(aes(label = ifelse(KGE < -1, round(KGE, 1), "")), y = 0.07, angle = 90) +
      coord_cartesian(ylim = c(ifelse(min(df$score, na.rm = TRUE) >= 0,
                                      0,
                                      ifelse(min(df$score, na.rm = TRUE) < -1,
                                             -1,
                                             min(df$score, na.rm = TRUE))),
                               1)) +
      scale_fill_gradient(name = expression(Q[mean]), low = "#eff3ff", high = "#2473B6",
                          guide = guide_colorbar(frame.colour = "black",
                                                 frame.linewidth = 1,
                                                 ticks = TRUE,
                                                 ticks.colour = "black",
                                                 ticks.linewidth = 1,
                                                 label.position = "bottom",
                                                 barwidth = 13,
                                                 barheight = 1.3,
                                                 direction = 'horizontal')) +
      scale_x_continuous(breaks = seq(floor(min(df$hyear, na.rm = TRUE) / 10) * 10,
                                      ceiling(max(df$hyear, na.rm = TRUE) / 10) * 10, 5)) +
      scale_y_continuous(breaks = c(0, 0.5, 1))

  }

  # Get output format
  crit <- match.arg(crit)

  # Create dataframe
  model <- data.frame(date, sim, obs)

  # Calculate hydrological year and flow subregimes
  lf_t <- quantile(obs, lf_quantile)
  hf_t <- quantile(obs, hf_quantile)

  x_all <- model |>
    dplyr::mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           hyear = dplyr::case_when(month < month_hyear ~ year - 1,
                             TRUE ~ year),
           flow_type = dplyr::case_when(obs <= lf_t ~ "low",
                                 obs >= hf_t ~ "high",
                                 TRUE ~ "all"))

  x_mean <- x_all |> dplyr::group_by(hyear) |> dplyr::summarise(mean = mean(obs, na.rm = TRUE))
  x_l <- x_all |> dplyr::filter(flow_type == "low")
  x_h <- x_all |> dplyr::filter(flow_type == "high")

  score <- dplyr::bind_rows(x_all |> dplyr::mutate(flow_type = "all"), x_l, x_h) |>
    dplyr::group_by(hyear, flow_type) |>
    dplyr::summarise(score = hydros1mple::score(sim, obs, crit = crit, sf = sf))

  # Generate plot

  ## hf_quantile
  x <- score |>
    dplyr::filter(flow_type == "high") |>
    dplyr::left_join(x_mean, by = "hyear")

  plot_h <- q_plot(x) +
    ylab(paste0(crit, "\n", ">Q", hf_quantile * 100)) +
    theme_bw(base_size = 16) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(8, 8, -8, 8, "pt"),
          legend.position = "none")

  ## lf_quantile
  x <- score |>
    dplyr::filter(flow_type == "low") |>
    dplyr::left_join(x_mean, by = "hyear")

  plot_l <- q_plot(x) +
    ylab(paste0(crit, "\n", "<Q", lf_quantile * 100)) +
    theme_bw(base_size = 16) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(-8, 8, -8, 8, "pt"),
          legend.position = "none")

  ## all flow
  x <- score |>
    dplyr::filter(flow_type == "all") |>
    dplyr::left_join(x_mean, by = "hyear")

  plot_all <- q_plot(x) +
    xlab(paste0("Hydrological year starting from ", month.name[month_hyear])) +
    ylab(paste0(crit, "\n", "Q")) +
    theme_bw(base_size = 16) +
    theme(plot.margin = margin(-8, 8, 8, 8, "pt"),
          legend.position = "bottom")

  # final plot
  cowplot::plot_grid(plot_h, plot_l, plot_all,
                     align = "v", ncol = 1, rel_heights = rel_heights)

}
