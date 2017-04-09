#' Add Dendrogram to Ordination Plot
#'
#' Modeled after the ordicluster function in vegan, this function overlays an ordination object with a cluster dendogram. Functionality has been added to include treatment groups.
#'
#' @param ord An ordination object.
#' @param cluster A cluster object from 'hclust' based on the same distance as 'ord.'
#' @param treatments A vector assigning treatments to samples.
#' @param choices Ordination axes to be plotted.
#' @param prune Number of upper level hierarchies removed from the dendrogram. If prune > 0, dendrogram will be disconnected.
#' @param col  A vector of cluster group memberships. Used to assign colors to line segments for each cluster group.
#' @param plot A logical; defaults to TRUE.
#'
#' @details 'treatments' should be a vector of class factor and length equal to the number of samples included in the ordination and cluster; integers are not coerced into factors.
#'
#' @author Jari Oksanen, John Quensen
#'
#' @return Invisibly returns a list of the data frames used to make the plot (df_ord, df_segments) and the plot itself (plot).
#' @export
#' @import vegan
#' @import ggplot2
#' @importFrom grDevices col2rgb
#' @importFrom stats weighted.mean
#' @importFrom stats weights
#' @examples
#' data(dune)
#' data(dune.env)
#' dune.dist <- vegdist(dune, method="bray")
#' ord <- metaMDS(dune, k=3)
#' cl <- hclust(dune.bray, method="complete")
#' gg_ordicluster(ord, cluster=cl, treatments=dune.env$Management, prune=3, col=cutree(cl, 4))
#'
gg_ordicluster <- function (ord, cluster, treatments=NA, choices=c(1,2), prune = 0, col = 1, plot=TRUE)
{
  if (is.numeric(treatments)) {
    stop("'treatments' cannot be numeric")
  }
  n.trts <- nlevels(treatments)
  if (n.trts==0) {
    treatments <- "none"
    n.trts <- 1
  }
  if (n.trts==1) {
    show.legend <- FALSE
  }
  else {
    show.legend <- TRUE
  }
  display <- "sites"
  w <- stats::weights(ord, display)
  weights.default <- function(object, ...) NULL
  w <- eval(w)
  mrg <- cluster$merge
  ord.scores <- scores(ord, display = display, choices=choices)
  if (nrow(mrg) != nrow(ord.scores) - 1)
    stop("Dimensions do not match in 'ord' and 'cluster'")
  if ((nrow(ord.scores) != length(treatments)) & (n.trts > 1))
    stop("Dimensions of 'ord' and 'treatments' do not match")
  if (length(w) == 1)
    w <- rep(w, nrow(ord.scores))
  n <- if (is.null(w))
    rep(1, nrow(ord.scores))
  else w
  noden <- numeric(nrow(mrg) - prune)
  go <- matrix(0, nrow(mrg) - prune, 2)
  col <- rep(col, length = nrow(ord.scores))
  col <- col2rgb(col)/255
  nodecol <- matrix(NA, nrow(mrg) - prune, 3)
  for (i in 1:(nrow(mrg) - prune)) {
    a <- mrg[i, 1]
    b <- mrg[i, 2]
    one <- if (a < 0)
      ord.scores[-a, ]
    else go[a, ]
    two <- if (b < 0)
      ord.scores[-b, ]
    else go[b, ]
    n1 <- if (a < 0)
      n[-a]
    else noden[a]
    n2 <- if (b < 0)
      n[-b]
    else noden[b]
    xm <- weighted.mean(c(one[1], two[1]), w = c(n1, n2))
    ym <- weighted.mean(c(one[2], two[2]), w = c(n1, n2))
    go[i, ] <- c(xm, ym)
    noden[i] <- n1 + n2
    colone <- if (a < 0)
      col[, -a]
    else nodecol[a, ]
    coltwo <- if (b < 0)
      col[, -b]
    else nodecol[b, ]
    nodecol[i, ] <- (n1 * colone + n2 * coltwo)/noden[i]

    # Rather than plotting the line segments, collect the coordinates and
    # color into a data frame.
    col.a = rgb(t(nodecol[i, ]))
    temp <- c(one[1], one[2], two[1], two[2], col.a)

    if (i==1){
      temp2 <- temp
    } else {
      temp2 <- rbind(temp2, temp)
      rownames(temp2) <- NULL # prevents duplicate row names
    }

  }

  colnames(temp2) <- c("x", "y", "xend", "yend", "Group")
  temp2 <- as.data.frame(temp2)
  j <- sapply(temp2, is.factor)
  temp2[j] <- lapply(temp2[j], as.character)
  j <- c( rep(TRUE, 4), FALSE)
  temp2[j] <- lapply(temp2[j], as.numeric)
  df_segments <- temp2

  df_ord <- as.data.frame(ord.scores)
  df_ord$Treatment <- treatments
  colnames(df_ord) <- c("x", "y", "Treatment")

  xlab <- paste("Axis", choices[1], sep=" ")
  ylab <- paste("Axis", choices[2], sep=" ")

  plt <- ggplot() +
    geom_segment(data=df_segments, aes(x=x, y=y, xend=xend, yend=yend),
                 color=df_segments$Group,
                 show.legend = FALSE) +
    geom_point(data=df_ord, aes(x=x, y=y, shape=Treatment), size=3,
               show.legend = show.legend) +
    xlab(xlab) + ylab(ylab)

  if (plot==TRUE) {print(plt)}

  invisible(list(df_ord=df_ord, df_segments=df_segments, plot=plt))
}
