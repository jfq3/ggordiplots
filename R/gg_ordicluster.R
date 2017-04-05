#' Ordicluter Plot with ggplot2
#'
#' @param ord An ordination object
#' @param clstr.rslt Result from hclust based on same distance as ordination
#' @param plot A logical; defaults to TRUE
#'
#' @return A list the plot and data frames used for plotting.
#' @export
#' @import ggplot2
#' @import vegan
#'
#' @examples
#' data("dune")
#' dune.bray <- vegdist(dune, method="bray")
#' ord <- cmdscale(dune.bray, k=nrow(dune)-1, eig=TRUE, add=TRUE)
#' clstr.rslt <- hclust(dune.bray, method="single")
#' gg_ordicluster(ord, clstr.rslt)
#'
gg_ordicluster <- function(ord, clstr.rslt, plot=TRUE) {
  ord.scores <- as.data.frame(scores(ord, display="sites", choices=c(1,2), scaling=1))
  ordi.rslt <- my.ordicluster(ord, clstr.rslt)
  mrg <- clstr.rslt$merge[ , c(1,2)]
  go <- ordi.rslt[ , c(1,2)]

  for (i in 1:(nrow(mrg))) {
    a <- mrg[i, 1]
    b <- mrg[i, 2]

    if (a < 0){
      one <- ord.scores[-a, ]
    } else {
      one <- go[a, ]
    }

    if (b < 0) {
      two <- ord.scores[-b, ]
    } else {
      two <- go[b, ]
    }

    temp <- c(one, two)
    # print(temp)
    if (i==1) {
      temp2 <- temp
    } else {
      temp2 <- rbind(temp2, temp)
    }

  }

  rownames(temp2) <- NULL
  temp2 <- apply(temp2, 2, unlist)
  colnames(temp2) <- c("x", "y", "xend", "yend")
  df_segments <- as.data.frame(temp2)
  df_ord <- ord.scores
  colnames(df_ord) <- c("x", "y")

  xlab <- "Axis 1"
  ylab <- "Axis 2"

  plt <- ggplot() +
    geom_point(data=df_ord, aes(x=x, y=y), size=3) +
    geom_segment(data=df_segments, aes(x=x, y=y, xend=xend, yend=yend)) +
    xlab(xlab) + ylab(ylab)

  if(plot){print(plt)}

  invisible(list(df_ord=df_ord, df_segments=df_segments, plot=plt))

}
