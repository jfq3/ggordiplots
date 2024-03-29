#' Scale Arrows to Plot
#'
#' Scales envfit arrows to fit within 75% of ordination plot dimensions.
#'
#' @param arrows A two column data frame of coordinates from envfit result.
#' @param  data A two column data frame of coordinates for ordination plot.
#' @param at coordinates of origin (0, 0)
#' @param fill proportion of plot area to fill with maximum arrow length
#'
#' @return Silently returns a data frame of scaled coordinates for adding arrows to ordination plot.
#'
#' @import vegan
#' @import ggplot2
#' @export
#' @author Jari Oksanen with modifications by Gavin Simpson and John Quensen
#'
#' @examples
#' data("varespec")
#' data("varechem")
#' vare_dist <- vegdist(varespec)
#' vare_mds <- monoMDS(vare_dist)
#' plt1 <- gg_envfit(ord=vare_mds, env=varechem, plot = FALSE)
#' mult <- scale_arrow(plt1$df_arrows, plt1$df_ord[ , c("x", "y")])
#' mult
#'
scale_arrow <- function(arrows, data, at = c(0, 0), fill = 0.75) {
  u <- c(range(data[,1], range(data[,2])))
  u <- u - rep(at, each = 2)
  r <- c(range(arrows[, 1], na.rm = TRUE), range(arrows[, 2], na.rm = TRUE))
  rev <- sign(diff(u))[-2]

  if (rev[1] < 0) {
    u[1:2] <- u[2:1]
  }
  if (rev[2] < 0) {
    u[3:4] <- u[4:3]
  }
  u <- u/r
  u <- u[is.finite(u) & u > 0]
  invisible(fill * min(u))
}
