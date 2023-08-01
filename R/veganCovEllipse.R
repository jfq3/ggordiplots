#' veganCovEllipse
#'
#' Generates points for ellipse around treatment centroids
#'
#' @param cov Covariance extracted from ordination with vegan's ordiellipse function
#' @param center Coordinates for center of ellipse
#' @param scale Ellipse dimension in standard deviations
#' @param npoints Number of points to generate
#' @return A dataframe for plotting ellipses around each group centroid.
#' @author Jari Oksanen

veganCovEllipse <-
  function(cov, center = c(0,0), scale = 1, npoints = 100)
  {
    ## Basically taken from the 'car' package: The Cirlce
    theta <- (0:npoints) * 2 * pi/npoints
    Circle <- cbind(cos(theta), sin(theta))
    ## scale, center and cov must be calculated separately
    Q <- chol(cov, pivot = TRUE)
    ## pivot takes care of cases when points are on a line
    o <- attr(Q, "pivot")
    t(center + scale * t(Circle %*% Q[,o]))
  }
