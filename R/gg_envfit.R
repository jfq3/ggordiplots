#' Vegan envfit plot
#'
#' Fits environmental parameters to an ordination plot of sites
#' and plots them as arrows.
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
#' @param plot A logical for plotting; defaults to TRUE.
#'
#' @return Silently returns the plot and data frames used for the plotting.
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
gg_envfit <- function(ord, env, groups=NA, scaling = 1, choices=c(1,2), perm = 999, alpha = 0.05, angle=20, len=0.5, unit="cm", arrow.col="red", pt.size=3, plot=TRUE) {
  df_ord <- vegan::scores(ord, display = "sites", choices = choices, scaling = scaling)
  df_ord <- as.data.frame(df_ord)
  axis.labels <- colnames(df_ord)
  if (!is.na(groups[1])) {
    df_ord$Group <- groups
    df_ord <- df_ord[ , c(3,1,2)]
    colnames(df_ord) <- c("Group", "x", "y")
  } else{
      colnames(df_ord) <- c("x", "y")
  }

  fit <- vegan::envfit(ord, env, choices = choices, perm = perm)
  df_arrows <- as.data.frame(scores(fit, "vectors"))
  mult <- vegan:::ordiArrowMul(fit)
  if (mult < 1) {
    df_arrows <- mult * df_arrows
  }
  df_arrows$var <- rownames(df_arrows)
  df_arrows$p.val <- fit$vectors$pvals
  colnames(df_arrows) <- c("x", "y", "var", "p.val")
  df_arrows <- df_arrows[df_arrows$p.val<=alpha, ]

  xlab <- axis.labels[1]
  ylab <- axis.labels[2]

  if (is.na(groups[1])) {
    plt <- ggplot(data=df_ord, aes(x=x, y=y)) + geom_point(size=pt.size) +
      xlab(xlab) + ylab(ylab)
  }
  else {
    plt <- ggplot(data=df_ord, aes(x=x, y=y, color=Group)) + geom_point(size=pt.size) +
      xlab(xlab) + ylab(ylab)
  }
  plt <- plt +
    geom_segment(data=df_arrows, aes(x=0, xend=x, y=0, yend=y),
                 arrow=arrow(angle=angle, length=unit(len, unit)), color=arrow.col) +
    geom_text(data=df_arrows, aes(x=x, y=y, label=var), color=arrow.col)

  # Plot?
  if (plot) {print(plt)}

  # Return data frames, plot as a list.
  invisible(list(df_ord=df_ord, df_arrows=df_arrows, plot=plt))
}
