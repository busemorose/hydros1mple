#' Plot discharge and precipitation on the same graph.
#'
#' @param date A date vector.
#' @param q A numeric vector for discharge.
#' @param p A numeric vector for precipitation.
#' @param maxRange A numeric value. Set the width of the discharge axis.
#' @param coeff A numeric value. Set the shrink coefficient of precipitation (visual ratio).
#' @param precip_breaks A numeric vector. Set the axis ticks values. Manual breaks allows to avoid truncated value caused by negative margin.
#' @param xlab A string value.
#' @param ylab A string value.
#' @param ylab2 A string value.
#' @param title A string value.
#' @param legend A string value for the position of the legend. Either "none" for no legend, or "bottom", "right", "top", "left".
#' @param date_breaks A string giving the formatting specification for the labels. Codes are defined in strftime().
#' @param date_labels A string giving the distance between breaks like "2 weeks", or "10 years". If both breaks and date_breaks are specified, date_breaks wins.
#'
#' @return A plot with discharge and precipitation.
#' @export
#' @import ggplot2
#'
#' @examples
#' data(KarstMod_dataset)
#'
#' plot2qp(KarstMod_dataset$date, KarstMod_dataset$QobsS, KarstMod_dataset$P,
#'         maxRange = 150, coef = 1.5)

plot2qp <- function(date,
                    q,
                    p,
                    maxRange = 20,
                    coeff = 1,
                    precip_breaks = c(0, 25, 50, 75, 100, 125, 150),
                    xlab = waiver(),
                    ylab = "Discharge",
                    ylab2 = "Precipitation",
                    title = waiver(),
                    legend = c("none", "bottom", "right", "top", "left"),
                    date_breaks = waiver(),
                    date_labels = waiver()) {

  # Get matching arguments
  legend <- match.arg(legend)

  # Create dataframe
  df <- data.frame(date, q, p)

  ggplot(df, aes(x = date)) +

    # Use geom_tile to create the inverted hyetograph
    # y = the center point of each bar
    # maxRange - Precipitation/coeff/2
    geom_tile(aes(y = maxRange - p/coeff/2,
                  height = p/coeff,
                  fill = "precip")) +
    # Plot discharge data
    geom_line(aes(y = q, color = "discharge")) +
    # Create a second axis with sec_axis() and format the labels
    # to display the original precipitation units
    scale_x_date(date_breaks = date_breaks, date_labels = date_labels,
                 expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(limits = c(0, maxRange),
                       expand = expansion(mult = c(0, 0)),
                       sec.axis = sec_axis(trans = ~(. - maxRange) * -coeff,
                                           name = ylab2,
                                           breaks = precip_breaks)) +
    scale_fill_manual(values = c("precip" = "blue"),
                      labels = c("precip" = "Precipitation"),
                      name = NULL) +
    scale_color_manual(values = c("discharge" = "black"),
                       labels = c("discharge" = "Observed Discharge"),
                       name = NULL) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position = legend)
}
