#' Identify consecutive dates.
#'
#' @param date A date vector.
#' @param interval A numeric value corresponding to the interval in days, hour, minute or seconds.
#' @param unit A string value corresponding to time unit, e.g. "days", "hours", "mins".
#'
#' @return A dataframe.
#' @import data.table
#' @export
#'
#' @examples
#' date <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-02-05", "2020-02-06", "2020-07-07"))
#' km_import(path = path)

date_consec <- function(date, interval, unit) {

  x <- data.table::data.table(date)[, consec := data.table::rleid(cumsum(c(FALSE, diff(date) > interval)))
  ][, by = "consec",
    .(min_date = min(date),
      max_date = max(date))
  ][, length := max_date - min_date + 1][]

  units(x$length) <- unit
  x$length <- format(round(x$length, 3), scientific = FALSE)

  return(x)
}
