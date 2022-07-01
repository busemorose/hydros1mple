#' Import a dataset in KarstMod input format
#'
#' @param path Path to file.
#' @param version KarstMod version.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "KarstMod_dataset.txt", package = "hydros1mple")
#' import_KarstMod(path = path)

import_KarstMod <- function(path, version = c(3, 2)) {
  x <- utils::read.delim(path)
  x$date <- as.Date(as.character(x$date), "%Y%m%d")
  return(x)
}
