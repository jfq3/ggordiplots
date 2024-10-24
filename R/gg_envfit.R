#' Vegan envfit plot
#'
#' Fits environmental parameters to an ordination plot of sites
#' and plots them as arrows or centroid labels.
#'
#' @param ord An ordination object.
#' @param env A data frame of environmental parameters.
#' @param groups A vector of groups.
#' @param scaling Scaling value for plot.
#' @param choices Axes to plot.
#' @param perm Number of permutations.
#' @param alpha Maximum alpha value to be included in plot.
#' @param angle Angle of arrow tips.
#' @param len Arrow tip length.
#' @param unit Unit for length ("cm", "in")
#' @param arrow.col Arrow color.
#' @param pt.size Symbol size.
#' @param show_factors A logical for including factors on the plot; defaults to FALSE.
#' @param plot A logical for plotting; defaults to TRUE.
#' @details
#' Vegan's envfit function fits environmental variables to an ordination.
#' If the variable is numeric, an arrow is fitted to the ordination plot
#' indicating the direction in which that variable increases.
#' If the variable is a factor (or character vector), vegan's envfit plotting
#' function treats levels of the factor as groups and adds labels for
#' the factor's level to the ordination plot at each group's centroid center.
#' This behavior is now enabled in gg_envfit() if show_factors is set to TRUE.
#'
#' @note In order for the arrow tips to be labeled with the names of the variables, they must be supplied as a matrix or data frame. If a single variable is supplied as a vector, the arrow tip will be labeled with "1". A way-around is to convert the vector to a data frame with the column named for the variable.
#'
#' @return Silently returns the plot and data frames used for the plotting if the fit of any variable is significant at alpha. Otherwise returns a message that no variable is significant.
#' @export
#' @import vegan
#' @import ggplot2
#'
#' @examples
#' data("varespec")
#' data("varechem")
#' vare.dist <- vegdist(varespec)
#' vare.mds <- monoMDS(vare.dist)
#' gg_envfit(ord=vare.mds, env=varechem)
#'
#' data("dune")
#' data("dune.env")
#' dune.dist <- vegdist(dune)
#' dune.mds <- monoMDS(dune.dist)
#' # A1 supplied as a vector
#' gg_envfit(dune.mds, env=dune.env$A1, groups=dune.env$Management)
#' # A1 supplied as a data frame
#' A1 <- as.data.frame(dune.env$A1)
#' colnames(A1) <- "A1"
#' gg_envfit(dune.mds, env=A1, groups=dune.env$Management)
#'

gg_envfit <- function(ord, env, groups=NA, scaling = 1, choices=c(1,2),
                      perm = 999, alpha = 0.05, angle=20, len=0.5,
                      unit="cm", arrow.col="red", pt.size=3,
                      show_factors=FALSE, plot=TRUE) {
  x <- y <- Group <- NULL
  # Make data frame for plotting ordination points
  df_ord <- vegan::scores(ord, display = "sites", choices = choices, scaling = scaling)
  df_ord <- as.data.frame(df_ord)
  axis.labels <- ord_labels(ord)[choices]
  if (is.na(groups[1])) {
    colnames(df_ord) <- c("x", "y")
  } else {
    df_ord$Group <- groups
    df_ord <- df_ord[ , c(3,1,2)]
    colnames(df_ord) <- c("Group", "x", "y")
  }
  # Fit environmental variables to the ordination as the object fit
  fit <- vegan::envfit(ord, env, choices = choices, perm = perm)

  # If there are factor or character vectors in env, make a dataframe
  # for adding their labels to the plot.
  df_factors <- scores(fit, "factors")
  if (!(is.null(df_factors))) {
    df_factors <- as.data.frame(df_factors)
    df_factors$labels <- rownames(df_factors)
    colnames(df_factors) <- c("x", "y", "labels")
  } else {
    df_factors <- NULL
   }

  # If numeric values for the fit are not significant, abort.
  # Else create a dataframe for plotting arrows.
  if (min(fit$vectors$pvals) > alpha) {
    print(paste("No variable significant at alpha <=", as.character(alpha), sep = " "))
  } else {
    df_arrows <- as.data.frame(scores(fit, "vectors"))
    mult <- scale_arrow(df_arrows, df_ord[ , c("x", "y")])
    df_arrows <- mult * df_arrows
    df_arrows$var <- rownames(df_arrows)
    df_arrows$p.val <- fit$vectors$pvals
    colnames(df_arrows) <- c("x", "y", "var", "p.val")
    df_arrows <- df_arrows[df_arrows$p.val<=alpha, ]

    xlab <- axis.labels[1]
    ylab <- axis.labels[2]

    # Make base ordination plot
    if (is.na(groups[1])) {
      plt <- ggplot(data=df_ord, aes(x=x, y=y)) + geom_point(size=pt.size) +
        xlab(xlab) + ylab(ylab)
    } else {
      plt <- ggplot(data=df_ord, aes(x=x, y=y, color=Group)) + geom_point(size=pt.size) +
        xlab(xlab) + ylab(ylab)
    }

    # Add arrows to the base ordination plot.
    plt <- plt +
      geom_segment(data=df_arrows, aes(x=0, xend=x, y=0, yend=y),
                   arrow=arrow(angle=angle, length=unit(len, unit)), color=arrow.col) +
      geom_text(data=df_arrows, aes(x=x, y=y, label=var), color=arrow.col, hjust="outward")

    # If df_factors is not null and show_factors is TRUE,
    # add factor labels to the plot.
    if ((!(is.null(df_factors))) && show_factors == TRUE) {
      plt <- plt +
        geom_text(data=df_factors, aes(x=x, y=y, label=labels),
                  inherit.aes = FALSE)
    }

    plt <- plt + coord_fixed(ratio=1)

    # Plot?
    if (plot) {print(plt)}

    # Return data frames, plot as a list.
    invisible(list(df_ord=df_ord, df_arrows=df_arrows,
                   df_factors=df_factors, plot=plt))
  }

}
