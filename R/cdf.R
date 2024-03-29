#' Cumulative distribution function (CDF)
#'
#' @param var A numeric vector.
#' @param unique A boolean for specifying if returning only unique values.
#'
#' @return A list with two numeric vectors: 1. Probability exceedance and 2. Associated var value
#' @export
#'
#' @examples
#' data(KarstMod_dataset)
#' cdf(KarstMod_dataset$Qobs)

cdf <- function(var, unique = FALSE) {

  if (unique) {

    var_ordered <- sort(unname(var))
    non.NA <- sum(!is.na(var_ordered))
    quant <- (1 : non.NA) / non.NA
    index_unique <- length(var_ordered) - match(unique(var_ordered), rev(var_ordered)) + 1
    var_ordered_unique <- var_ordered[index_unique]
    quant <- quant[index_unique]

    return(list("prob_ex" = 1 - quant, "value" = var_ordered_unique))

  } else {

    y <- sort(var[!is.na(var)])
    x <- 1 - (seq_along(y) / length(y))

    return(list("prob_ex" = x, "value" = y))
  }

}
