#' Random sample of colors with/without grey scale
#'
#' @param n A number. Number of unique colors desired.
#' @param grey A boolean. FALSE remove all grey-like colors from the pool.
#'
#' @return A string vector of color names.
#' @export
#'
#' @examples
#' sample_color(5)
#' sample_color(5, grey = TRUE)

sample_color <- function(n, grey = FALSE) {

  if (grey) return(sample(grDevices::colors(), n))

  return(sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], n))

}

