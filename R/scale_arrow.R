#' scale arrows to plot
#'
#' Scales envfit arrows to fit within 75% of ordination plot dimendions.
#'
#' @param arrows A dataframe with two columns giving coordinates from envfit result.
#' @param  data A two column mata frame giving coordinates extracted from ordination with vegan scores function.
#'
#' @return Silently returns a scaled data fram for plottin arrows.
#'
#' @import vegan
#' @import ggplot2
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
