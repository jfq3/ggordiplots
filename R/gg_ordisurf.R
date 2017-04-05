#' Ordisurf with ggplot2
#'
#' Fits a surface (contour) plot of an environmental variable
#' to an ordination plot.
#'
#' @param ord An ordination object.
#' @param env.var Environmental variable to fit to plot.
#' @param choices Axes to plot.
#' @param var.label Label for the legend; default is "Level."
#' @param binwidth Controls the number of countours in the plot.
#' @param plot A logical for plotting; defaults to TRUE.
#'
#' @return Silently returns the plot and data frames used for the plotting.
#' @export
#'
#' @import ggplot2
#' @import vegan
#' @importFrom stats na.omit
#'
#' @examples
#' data(varespec)
#' data(varechem)
#' vare.dist <- vegdist(varespec)
#' vare.mds <- monoMDS(vare.dist)
#' gg_ordisurf(vare.mds, env.var = varechem$Baresoil, var.label="Bare Soil")

gg_ordisurf <- function(ord, env.var, choices=c(1,2), var.label="Level", binwidth=2, plot=TRUE) {
  # Extract ordisurf data for plotting
  ordi <- vegan::ordisurf(ord ~ env.var, plot=FALSE) #created the ordisurf object
  ordi.grid <- ordi$grid #extracts the ordisurf object
  ordi.data <- expand.grid(x = ordi.grid$x, y = ordi.grid$y) #get x and ys
  ordi.data$z <- as.vector(ordi.grid$z) #unravel the matrix for the z scores
  df_surf <- data.frame(na.omit(ordi.data)) #gets rid of the nas

  # Extract site coordinates for plotting.
  df_ord <- as.data.frame(scores(ord))
  colnames(df_ord) <- c("x", "y")

  # Make axis labels.
  xlab <- paste("Axis", choices[1], sep=" ")
  ylab <- paste("Axis", choices[2], sep=" ")

  ## Plotting in ggplot2
  plt <- ggplot(data=df_ord, aes(x=x, y=y)) + geom_point() +
    xlab(xlab) + ylab(ylab) +
    stat_contour(data = df_surf, aes(x = x, y = y, z = z, colour = ..level..), binwidth = binwidth) +
    labs(color=var.label)
  #can change the binwidth depending on how many contours you want

  # Plot?
  if (plot) {print(plt)}

  # Return data frames, plot as a list.
  invisible(list(df_ord=df_ord, df_surf=df_surf, plot=plt))
}
