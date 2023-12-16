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
#' km_import(path = path)

km_import <- function(path, version = c("3", "2")) {

  # Get matching arguments
  version <- match.arg(version)

  # Import dataset
  if (version == "3") {

    x <- utils::read.delim(path, na.strings = c("NA", "INTERP", "NOINTERP"))
    x$date <- as.Date(as.character(x$date), "%Y%m%d")
    return(x)

  } else if (version == "2") {

    x <- utils::read.delim(path, na.strings = c("NA", "INTERP", "NOINTERP"))
    names(x)[names(x) == "X.date"] <- "date"
    x$date <- as.Date(as.character(x$date), "%Y%m%d")
    return(x)

  }
}
