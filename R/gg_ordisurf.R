#' Ordisurf with ggplot2
#'
#' Fits a surface (contour) plot of an environmental variable
#' to an ordination plot.
#'
#' @param ord An ordination object.
#' @param env.var Environmental variable to fit to plot.
#' @param groups A vector of groups (optional).
#' @param choices Axes to plot.
#' @param var.label Label for the contour legend; default is "Level."
#' @param binwidth Controls the number of contours in the plot.
#' @param pt.size Symbol size.
#' @param family Error distribution and link function used by the gam function to fit the contours.
#' @param plot A logical for plotting; defaults to TRUE.
#'
#' @details By default, `binwidth` is calculated as the difference between minimum and maximum values of the variable divided by 15.
#' @details The colors for the points are mapped to fill; if you want to rename the legend for the groups (points), use labs(fill="New name").
#' @details See the help for stats::family for possible values for family.
#'
#' @return Silently returns the plot and data frames used for the plotting.
#' @export
#'
#' @import ggplot2
#' @import glue
#' @import vegan
#' @importFrom stats na.omit
#' @importFrom grDevices rgb
#' @importFrom stats var
#'
#' @note Code for extracting plot data from the ordisurf result was taken from a blog by Oliviea Rata Burge.
#' @author Olivia Rata Burge, John Quensen
#' @references https://oliviarata.wordpress.com/2014/07/17/ordinations-in-ggplot2-v2-ordisurf/'
#' @examples
#' data(varespec)
#' data(varechem)
#' vare.dist <- vegdist(varespec)
#' vare.mds <- monoMDS(vare.dist)
#' gg_ordisurf(vare.mds, env.var = varechem$Baresoil, var.label="Bare Soil")

gg_ordisurf <- function(ord, env.var, groups=NA, choices=c(1,2), var.label="Level",
                        binwidth, pt.size=3, family = "gaussian", plot=TRUE) {
  x <- y <- z <- Group <- ..level.. <- NULL
  groups <- as.factor(groups)

  # Extract ordisurf data for plotting
  ordi <- vegan::ordisurf(ord ~ env.var, family = family, plot=FALSE) # creates the ordisurf object
  ordi.grid <- ordi$grid # extracts the ordisurf object
  ordi.data <- expand.grid(x = ordi.grid$x, y = ordi.grid$y) # get x and y
  ordi.data$z <- as.vector(ordi.grid$z) # unravel the matrix for the z scores
  df_surf <- data.frame(na.omit(ordi.data)) # gets rid of the NAs

  # Extract site coordinates for plotting.
  df_ord <- as.data.frame(scores(ord, choices = choices, display = "sites"))
  if (is.na(groups)[1]) {
    df_ord <- data.frame(x=df_ord[ , 1], y=df_ord[ , 2])
  } else {
    df_ord <- data.frame(x=df_ord[ , 1], y=df_ord[ , 2], Group=groups)
  }

  # Make axis labels.
  axis.labels <- ord_labels(ord)[choices]
  xlab <- axis.labels[1]
  ylab <- axis.labels[2]

  # Calculate default binwidth
  # Can change the binwidth depending on how many contours you want
  if(missing(binwidth)) {
    r <- range(env.var)
    binwidth <- (r[2]-r[1])/15
  } else {
    binwidth = binwidth
  }

  # Plotting in ggplot2
  if (is.na(groups)[1]) {
    plt <- ggplot() +
      geom_point(data=df_ord, aes(x=x, y=y), size=pt.size) +
      xlab(xlab) + ylab(ylab) +
      stat_contour(data=df_surf, aes(x=x, y=y, z=z, color= ..level..), binwidth=binwidth) +
      labs(color=var.label) +
      coord_fixed(ratio=1)
  } else {
    plt <- ggplot() +
      geom_point(data=df_ord, aes(x=x, y=y, fill=Group), shape=21, color="#00000000", size=pt.size) +
      xlab(xlab) + ylab(ylab) +
      stat_contour(data=df_surf, aes(x=x, y=y, z=z, color= ..level..), binwidth=binwidth) +
      labs(color=var.label) +
      coord_fixed(ratio=1)
  }

  # Plot?
  if (plot) {print(plt)}

  # Return data frames, plot as a list.
  invisible(list(df_ord=df_ord, df_surf=df_surf, plot=plt))
}
