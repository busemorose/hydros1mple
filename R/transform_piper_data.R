#' Transform analyses to coordinates for Piper diagram. This function already integrates the conversion to meq/L and to percent.
#'
#' @param data A dataframe with ions as columns.
#' @param ions A string vector of ions to convert.
#' @param cations A string vector of cations.
#' @param anions A string vector of anions.
#'
#'
#' @return A dataframe with three columns, including "x" and "y" which correspond to the x and y coordinates of the blank piper ggplot..
#' @export
#'
#'
#' @examples
#' data(pc_data)
#'
#' transform_piper_data(pc_data,
#'                      ions = c("Ca", "Mg", "Na", "K", "SO4", "Cl", "HCO3"),
#'                      anions = c("Cl", "SO4", "HCO3"))

transform_piper_data <- function(data,
                                 ions = c("Ca", "Mg", "Na", "K", "Cl", "SO4", "HCO3", "CO3"),
                                 cations = c("Ca", "Mg", "Na", "K"),
                                 anions = c("Cl", "SO4", "HCO3", "CO3"),
                                 name = NULL) {

  piper_data <- data |>
    mgl_to_meq(ions = ions) |>
    meq_to_percent(cations = cations, anions = anions)

  Mg <- piper_data$Mg_meq_p
  Ca <- piper_data$Ca_meq_p
  Cl <- piper_data$Cl_meq_p
  SO4 <- piper_data$SO4_meq_p

  if (is.null(name)) {
    name = rep(1:length(Mg), 3)
  } else {
    name = rep(name, 3)
  }

  y1 <- Mg * 0.86603
  x1 <- 100 * (1 - (Ca / 100) - (Mg / 200))
  y2 <- SO4 * 0.86603
  x2 <-120 + (100 * Cl / 100 + 0.5 * 100 * SO4 / 100)

  new_point <- function(x1, x2, y1, y2, grad = 1.73206) {
    b1 <- y1 - (grad * x1)
    b2 <- y2 - ( - grad * x2)
    M <- matrix(c(grad, -grad, -1, -1), ncol = 2)
    intercepts <- as.matrix(c(b1, b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x = t_mat[1, 1], y = t_mat[2, 1])
  }

  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind", np_list)
  data.frame(observation = name, x = c(x1, x2, npoints$x), y = c(y = y1, y2, npoints$y))
}
