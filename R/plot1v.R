#' Plot one variable.
#'
#' @param date A date vector.
#' @param data A numeric vector.
#' @param var A string value. Either "q" for discharge, "p" for precipitation, "t" for temperature, "et" for evapotranspiration.
#' @param group A string value for grouping data. Either NA for no group, or "hour", "day", "month" or "year".
#' @param type A string value for aggregate type. Either "mean" or "sum".
#' @param xlab A string value.
#' @param ylab A string value.
#' @param title A string value.
#' @param date_breaks A string giving the formatting specification for the labels. Codes are defined in strftime().
#' @param date_labels A string giving the distance between breaks like "2 weeks", or "10 years". If both breaks and date_breaks are specified, date_breaks wins.
#'
#' @return A plot.
#' @export
#' @import ggplot2
#'
#' @examples
#' data(KarstMod_dataset)
#'
#' # Basic hydrograph
#'
#' plot1v(KarstMod_dataset$date, KarstMod_dataset$QobsS, var = "q")
#'
#' # Add details
#'
#' plot1v(KarstMod_dataset$date, KarstMod_dataset$QobsS, var = "q",
#'        xlab = "date", ylab = "discharge", title = "Hydrograph",
#'        date_breaks = "3 month", date_labels = "%Y-%m")

plot1v <- function(date,
                   data,
                   var = c("q", "p", "t", "et"),
                   group = c(NA, "hour", "day", "month", "year"),
                   type = c("mean", "sum"),
                   xlab = waiver(),
                   ylab = waiver(),
                   title = waiver(),
                   date_breaks = waiver(),
                   date_labels = waiver()) {

  # Get matching arguments
  var <- match.arg(var)
  group <- match.arg(group)
  type <- match.arg(type)

  # Create dataframe
  df <- data.frame(date, data)

  # Grouping workflow
  if (!is.na(group)) {
    df$year <- lubridate::year(date)
    df$month <- lubridate::month(date)
    df$day <- lubridate::day(date)
    df$hour <- lubridate::hour(date)

    # Get appropriate grouping vector
    group <- switch(group,
                    "hour" = c("hour", "day", "month", "year"),
                    "day" = c("day", "month", "year"),
                    "month" = c("month", "year"),
                    "year" = "year")

    # Summarise data
    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
      dplyr::summarise(data = dplyr::case_when(type == "mean" ~ mean(data),
                                               type == "sum" ~ sum(data)))

    # Generate missing date elements
    df <- df |>
      dplyr::mutate(day = if("day" %in% colnames(df)) day else 1,
                    month = if("month" %in% colnames(df)) month else 1,
                    hour = if("hour" %in% colnames(df)) hour else 1)

    # Recreate either date or datetime vector
    if ("hour" %in% group) {
      df <- df |> dplyr:: mutate(date = lubridate::make_datetime(year, month, day, hour))
    } else {
      df <- df |> dplyr::mutate(date = lubridate::make_date(year, month, day))
    }
  }

  # Get maximum data value for plot y-axis upper limit
  hlim <- max(df$data, na.rm = TRUE)

  # Q, ET plots (geom_line, scale_y_continuous for removing white space below 0)
  if (var %in% c("q", "et")) {
    x <- ggplot(df, aes(date, data)) +
      geom_line() +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels,
                   expand = expansion(mult = c(0.01, 0.01))) +
      scale_y_continuous(limits = c(0, hlim), expand = expansion(mult = c(0, 0.1)))
  }

  # T plot (geom_line)
  if (var == "t") {
    x <- ggplot(df, aes(date, data)) +
      geom_line() +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels,
                   expand = expansion(mult = c(0.01, 0.01)))
  }

  # P plot (geom_bar, scale_y_continuous for removing white space below 0)
  if (var == "p") {
    x <- ggplot(df, aes(date, data)) +
      geom_bar(stat = "identity", fill = "blue") +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels,
                   expand = expansion(mult = c(0.01, 0.01))) +
      scale_y_continuous(limits = c(0, hlim), expand = expansion(mult = c(0, 0.1)))
  }

  # Add axis and title labels + theme
  x +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    theme_bw()
}
