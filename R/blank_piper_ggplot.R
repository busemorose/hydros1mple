#' Plot a blank Piper diagram with ggplot.
#'
#'
#'
#' @return A blank ggplot Piper diagram.
#' @export
#' @import ggplot2
#'
#' @examples
#' blank_ggplot_piper()

blank_ggplot_piper <- function() {

  grid1p1 <- data.frame(x1 = c(20, 40, 60,80),
                        x2 = c(10, 20, 30, 40),
                        y1 = c(0, 0, 0, 0),
                        y2 = c(17.3206, 34.6412, 51.9618, 69.2824))

  grid1p2 <- data.frame(x1 = c(20, 40, 60, 80),
                        x2 = c(60, 70, 80, 90),
                        y1 = c(0, 0, 0, 0),
                        y2 = c(69.2824, 51.9618, 34.6412, 17.3206))

  grid1p3 <- data.frame(x1 = c(10, 20, 30, 40),
                        x2 = c(90, 80, 70, 60),
                        y1 = c(17.3206, 34.6412, 51.9618, 69.2824),
                        y2 = c(17.3206, 34.6412, 51.9618, 69.2824))

  grid2p1 <- grid1p1
  grid2p1$x1 <- grid2p1$x1 + 120
  grid2p1$x2 <- grid2p1$x2 + 120

  grid2p2 <- grid1p2
  grid2p2$x1 <- grid2p2$x1 + 120
  grid2p2$x2 <- grid2p2$x2 + 120

  grid2p3 <- grid1p3
  grid2p3$x1 <- grid2p3$x1 + 120
  grid2p3$x2 <- grid2p3$x2 + 120

  grid3p1 <- data.frame(x1 = c(100, 90, 80, 70),
                        y1 = c(34.6412, 51.9618, 69.2824, 86.603),
                        x2 = c(150, 140, 130, 120),
                        y2 = c(121.2442, 138.5648, 155.8854, 173.2060))

  grid3p2 <- data.frame(x1 = c(70, 80, 90, 100),
                        y1 = c(121.2442, 138.5648, 155.8854, 173.2060),
                        x2 = c(120, 130, 140, 150),
                        y2 = c(34.6412, 51.9618, 69.2824, 86.603))

  # Plot
  ggplot() +

    ## Left hand ternary plot
    geom_segment(aes(x = 0,y = 0, xend = 100, yend = 0)) +
    geom_segment(aes(x = 0,y = 0, xend = 50, yend = 86.603)) +
    geom_segment(aes(x = 50,y = 86.603, xend = 100, yend = 0)) +

    ## Right hand ternary plot
    geom_segment(aes(x = 120,y = 0, xend = 220, yend = 0)) +
    geom_segment(aes(x = 120,y = 0, xend = 170, yend = 86.603)) +
    geom_segment(aes(x = 170,y = 86.603, xend = 220, yend = 0)) +

    ## Upper diamond
    geom_segment(aes(x = 110,y = 190.5266, xend = 60, yend = 103.9236)) +
    geom_segment(aes(x = 110,y = 190.5266, xend = 160, yend = 103.9236)) +
    geom_segment(aes(x = 110,y = 17.3206, xend = 160, yend = 103.9236)) +
    geom_segment(aes(x = 110,y = 17.3206, xend = 60, yend = 103.9236)) +

    ## Add grid lines to the plots
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid1p1,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid1p2,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid1p3,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid2p1,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid2p2,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid2p3,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid3p1,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                 data = grid3p2,
                 linetype = "dashed",
                 size = 0.25,
                 colour = "grey50") +

    # Labels and grid values
    geom_text(aes(c(20, 40, 60, 80), c(-5, -5, -5, -5), label = c(80, 60, 40, 20)),
              size = 3) +
    geom_text(aes(c(35, 25, 15, 5), grid1p2$y2, label = c(80, 60, 40, 20)),
              size = 3) +
    coord_equal(ratio = 1) +
    geom_text(aes(c(215, 205, 195, 185), grid2p3$y2, label = c(20, 40, 60, 80)),
              size = 3) +
    geom_text(aes(c(140, 160, 180, 200), c(-5, -5, -5, -5), label = c(20, 40, 60, 80)),
              size = 3) +
    geom_text(aes(grid3p1$x1 - 5, grid3p1$y1 - 17.5, label = c(80, 60, 40, 20)),
              size = 3) +
    geom_text(aes(grid3p1$x2 + 5, grid3p1$y2, label = c(20, 40, 60, 80)),
              size = 3) +
    geom_text(aes(grid3p2$x1 - 5, grid3p2$y1, label = c(20, 40, 60, 80)),
              size = 3) +
    geom_text(aes(grid3p2$x2 + 5, grid3p2$y2 - 17.5, label = c(80, 60, 40, 20)),
              size = 3) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank()) +

    # Labels
    geom_text(aes(17, 50, label = "Mg^'2+'"),
              angle = 60,
              size = 3.5,
              parse = TRUE) +
    geom_text(aes(85.5, 50, label = "Na^'+'~+~K^'+'"),
              angle = -60,
              size = 3.5,
              parse = TRUE) +
    geom_text(aes(50, -10, label = "Ca^'2+'"),
              size = 3.5,
              parse = TRUE) +
    geom_text(aes(170, -10, label = "Cl^'-'"),
              size = 3.5,
              parse = TRUE) +
    geom_text(aes(205, 50, label = "SO[4]^'2-'"),
              angle = -60,
              size = 3.5,
              parse = TRUE) +
    geom_text(aes(136, 50, label = "CO[3]^'2-'~+~HCO[3]^'-'"),
              angle = 60,
              size = 3.5,
              parse = TRUE) +
    geom_text(aes(72.5, 150, label = "SO[4]^'2-'~+~Cl^'-'"),
              angle = 60,
              size = 3.5,
              parse = TRUE) +
    geom_text(aes(147.5, 150, label = "Ca^'2+'~+~Mg^'2+'"),
              angle = -60,
              size = 3.5,
              parse = TRUE)
}
