#' Frequency barplot of variables
#'
#' Warning: at the moment, only works for variables with min value greater than 10
#'
#' @param var_list A list of named vector.
#' @param interval An integer corresponding to the grouping interval.
#' @param x_name A string for the x-axis name.
#' @param color_low A color for small frequencies.
#' @param color_high A color for high frequencies.
#' @param legend_name A string for the name of the legend.
#' @param rect_width A value for the linewidth of the rectangle outline.
#'
#' @return A plot.
#' @export
#'
#' @examples
#' plot_freq(var_list = c("site_name" = c(1, 2, 4, 5, 6, 5, 5, 5), "site_name2" = c(2, 3, 5, 10, 15, 15, 15),
#'           interval = 2, x_name = "var")

plot_freq <- function(var_list, interval, x_name, basesize = 16, height = 0.3,
                      color_low = "lightyellow", color_high = "darkred", legend_name = "Freq", rect_width = 0.1) {

  df <- var_list |>
    purrr::imap(\(x, y) {
      total <- length(x)
      min_var <- min(x, na.rm = TRUE)
      max_var <- max(x, na.rm = TRUE)
      r_min_var <- ceiling(min_var / 10) * 10
      r_max_var <- floor(max_var / 10) * 10
      breaks <- c(min_var, seq(min_var, max_var, interval), max_var)

      df_n <- data.frame(var = x, name = y) |>
        mutate(int = findInterval(x, unique(breaks))) |>
        group_by(int) |>
        summarise(n = n())

      data.frame(int = 1:length(unique(breaks))) |>
        left_join(df_n, by = 'int') |>
        mutate(n = ifelse(is.na(n), 0, n)) |>
        mutate(freq = n / total) |>
        mutate(interval_var_min = unique(breaks)[int],
               interval_var_max = unique(breaks)[int] + interval) |>
        mutate(name = y, min_var, max_var)
    }) |>
    bind_rows()

  df |>
    ggplot(aes(x = interval_var_min, y = name)) +
    geom_hline(aes(yintercept = name)) +
    geom_point(color = "transparent") +
    geom_rect(aes(xmin = interval_var_min, xmax = interval_var_max,
                  ymin = as.integer(as.factor(name)) - height,
                  ymax = as.integer(as.factor(name)) + height,
                  fill = freq)) +
    geom_rect(aes(xmin = min_var, xmax = max_var + interval,
                  ymin = as.integer(as.factor(name)) - height,
                  ymax = as.integer(as.factor(name)) + height),
              fill = NA, color = "black", linewidth = rect_width) +
    scale_fill_gradient(low = color_low, high = color_high, name = legend_name,
                        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
    xlab(x_name) +
    theme_bw(base_size = basesize) +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank())
}

