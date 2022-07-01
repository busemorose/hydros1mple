#' Import a dataset in KarstMod input format
#'
#' @param path Path to file.
#' @param version KarstMod version.
#'
#' @return A dataframe.
#' @export
#'
#' @examples

import_KarstMod <- function(path, version = c(3, 2)) {
  x <- read.delim("R/KarstMod_dataset.txt")

  x$date <- as.Date(as.character(x$date), "%Y%m%d")

  return(x)
}
