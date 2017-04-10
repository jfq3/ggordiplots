#' Plot with Ellipses, Hulls, Spiders
#'
#' \code{gg_ordiplot} uses \code{ggplot2} to make an ordination plot
#' with group ellipses by default, and optionally hulls and/or
#' spiders. It is patterned after \code{vegan}'s functions \code{ordiellipse},
#' \code{ordihull}, and \code{ordispider} and accepts similar parameters.
#' @param ord An ordination object.
#' @param groups A vector of groups.
#' @param scaling Scaling for ordination plot.
#' @param choices Axes to be plotted.
#' @param kind Type of ellipses to show ("se", sd", "ehull").
#' @param conf Confidence value for ellipses if "se" or "sd."
#' @param show.groups Subset of groups to plot.
#' @param ellipse A logical for plotting ellipses; defaults to TRUE.
#' @param label A logical for labeling group centroids.
#' @param hull A logical for plotting group hulls.
#' @param spiders A logical for plotting group spiders.
#' @param plot A logical for plotting; defaults to TRUE.
#'
#' @return
#' Silently returns the plot and data frames used for the plotting.
#' @export
#' @import ggplot2
#' @import vegan
#' @importFrom stats aggregate
#'
#' @examples
#' data("dune")
#' data("dune.env")
#' dune.hel <- decostand(dune, method = "hellinger")
#' ord <- rda(dune.hel)
#' gg_ordiplot(ord, groups = dune.env$Management)
#'
gg_ordiplot <- function(ord, groups, scaling = 1, choices = c(1,2), kind = c("sd", "se", "ehull"), conf=NULL, show.groups="all", ellipse = TRUE, label = FALSE, hull = FALSE, spiders = FALSE, plot=TRUE) {

  if (show.groups[1]=="all") {
    show.groups <- as.vector(levels(groups))
  }

  # Get site coordinates to plot.
  df_ord <- vegan::scores(ord, display = "sites", scaling=scaling, choices=choices)
  axis.labels <- colnames(df_ord)
  df_ord <- data.frame(x=df_ord[ , 1], y=df_ord[ , 2], Group=groups)

  # Get ellipse centers to annotate.
  df_mean.ord <- aggregate(df_ord[,1:2], by=list(df_ord$Group),mean)
  colnames(df_mean.ord) <- c("Group", "x", "y")
  df_mean.ord <- df_mean.ord[df_mean.ord$Group %in% show.groups, ]

  # Get parameters from the ordiellipse function.
  if (is.null(conf)) {
    rslt <- vegan::ordiellipse(ord, groups=groups, display = "sites", scaling=scaling, choices=choices, kind = kind, show.groups = show.groups, draw = "none", label = label)
  } else {
      rslt <- vegan::ordiellipse(ord, groups=groups, display = "sites", scaling=scaling, choices=choices, kind = kind, show.groups = show.groups, draw = "none", conf = conf, label = label)
  }

  # Get points to plot for the ellipses.
  df_ellipse <- data.frame()
  for(g in show.groups) {
    df_ellipse <- rbind(df_ellipse, cbind(as.data.frame(with(df_ord[df_ord$Group==g,],
    vegan:::veganCovEllipse(rslt[[g]]$cov,rslt[[g]]$center, rslt[[g]]$scale))),Group=g))
  }
  colnames(df_ellipse) <- c("x", "y", "Group")
  df_ellipse <- df_ellipse[ , c(3,1,2)]

  # Make data frame for hulls.
  rslt.hull <- vegan::ordihull(ord, groups = groups, scaling = scaling, choices = choices, show.groups = show.groups, draw = "none")
  df_hull <- data.frame()
  df_temp <- data.frame()
  for (g in show.groups) {
    x <- rslt.hull[[g]][ , 1]
    y <- rslt.hull[[g]][ , 2]
    Group <- rep(g, length(x))
    df_temp <- data.frame(Group = Group, x=x, y=y)
    df_hull <- rbind(df_hull, df_temp)
  }

  # Make a data frame for the spiders.
  df_spiders <- df_ord
  df_spiders$cntr.x <- NA
  df_spiders$cntr.y <- NA
  for (g in show.groups) {
    df_spiders[which(df_spiders$Group==g), 4:5] <- df_mean.ord[which(df_mean.ord==g), 2:3]
  }
  df_spiders <- df_spiders[ , c(3,4,5,1,2)]
  df_spiders <- df_spiders[order(df_spiders$Group), ]
  df_spiders <- df_spiders[df_spiders$Group %in% show.groups, ]

  # Make basic ggplot with ellipses.
  xlab <- axis.labels[1]
  ylab <- axis.labels[2]
  plt <- ggplot2::ggplot() +
    geom_point(data=df_ord, aes(x=x, y=y, color=Group), size=3) +
    xlab(xlab) + ylab(ylab)

  # Add ellipses.
  if (ellipse == TRUE) {
    plt <- plt + geom_path(data = df_ellipse, aes(x=x, y=y, color=Group), show.legend = FALSE)
  }

  # Add labels.
  if (label == TRUE) {
    plt <- plt + geom_text(data=df_mean.ord, aes(x=x, y=y, label=Group, color=Group), show.legend = FALSE)
  }

  # Add hulls.
  if (hull == TRUE) {
    plt <- plt + geom_path(data=df_hull, aes(x=x, y=y, color=Group), show.legend = FALSE)
  }

  # Add spiders.
  if (spiders == TRUE) {
    plt <- plt + geom_segment(data=df_spiders, aes(x=cntr.x, xend=x, y=cntr.y, yend=y, color=Group), show.legend = FALSE)
  }

  plt <- plt + coord_fixed(ratio=1)

  # Plot?
  if (plot) {print(plt)}

  # Return data frames, plot as a list.
  invisible(list(df_ord=df_ord, df_mean.ord=df_mean.ord, df_ellipse=df_ellipse, df_hull=df_hull, df_spiders=df_spiders, plot=plt))
}
