#' Get the corresponding KarstMod index to a date
#'
#' @param date_begin Date object in %Y-%m-%d format.
#' @param date_end Date object in %Y-%m-%d format.
#'
#' @return A value corresponding to the index.
#' @export
#'
#' @examples
#' km_date_index(as.Date("2021-01-01"), as.Date("2022-01-01"))

km_date_index <- function(date_begin, date_end) {
  length(padr::span_date(format(date_begin, "%Y%m%d"), format(date_end, "%Y%m%d")))
}
