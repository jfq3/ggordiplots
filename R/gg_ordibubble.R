#' Ordination Bubble Plot
#'
#' Makes a simple ordination plot of site with the symbol size
#' scaled to an environmental variable. Result is similar to
#' that of BiodiversityR's ordibubble function.
#'
#' @param ord An ordination object
#' @param env.var An environmental variable.
#' @param groups A vector of groups (optional).
#' @param var.label Label for the legend; default is "Level."
#' @param choices Axes to be plotted.
#' @param plot A logical for plotting; defaults to TRUE.
#'
#' @return Silently returns the plot and data frames used for the plotting.
#' @export
#' @import ggplot2
#' @import vegan
#'
#' @examples
#' data(dune)
#' data(dune.env)
#' dune.bray <- vegdist(dune, method = "bray")
#' ord <- cmdscale(dune.bray, k=(nrow(dune)-1), eig=TRUE, add=TRUE)
#' gg_ordibubble(ord, env.var=dune.env$A1, var.label="A1")
#'
gg_ordibubble <- function(ord, env.var, groups=NA, var.label="Level", choices=c(1,2), plot=TRUE) {
  df_ord <- as.data.frame(vegan::scores(ord, display="sites", choices=choices))

  axis.labels <- ord_labels(ord)[choices]
  xlab <- axis.labels[1]
  ylab <- axis.labels[2]

  if (!is.na(groups)[1]) {
    df_ord$Group <- as.factor(groups)
    df_ord$var <- env.var
    colnames(df_ord) <- c("x", "y", "Group", var.label)
    plt <- ggplot(data=df_ord, aes(x=x, y=y, size=env.var, color=Group)) +
      geom_point() +
      xlab(xlab) +
      ylab(ylab) +
      labs(size=var.label) +
      coord_fixed(ratio=1)
  } else {
    df_ord$var <- env.var
    colnames(df_ord) <- c("x", "y", var.label)
    plt <- ggplot(data=df_ord, aes(x=x, y=y, size=env.var)) +
      geom_point() +
      xlab(xlab) +
      ylab(ylab) +
      labs(size=var.label) +
      coord_fixed(ratio=1)
  }

  # Plot?
  if (plot) {print(plt)}

  # Return data frames, plot as a list.
  invisible(list(df_ord=df_ord, plot=plt))
}
