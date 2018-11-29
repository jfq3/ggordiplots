#' Make Ordination Axis Labels
#'
#' Makes ordination axis labels that include, if apprpriate, the \% total variance explained by each axis.
#'
#' @param ord A vegan ordination object.
#'
#' @return A character vector, each element of which can be used to label the corresponding axis of an ordination plot.
#'
#' @details If there are no eigenvalues in ord, or if any eigenvalues are less than 0, each element of the vector returned has the form "DIMn" where n is the axis number. Otherwise, each element of the vector returned has the form "AxisN xx.x\%" where "Axis" is taken from the vector of eigenvalues in ord if they are named or simply "DIM" if they are not, N is the number of the axis, and xx.x is the \% of total variance explained by the axis.
#'
#'
#' @examples
#'
#'
ord_labels <-
  function(ord){
    ev <- vegan::eigenvals(ord)
    tol <- -(1e-07)*ev[1]
    ord.labels <- rep("", length(ev))
    if ((any(is.na(ev))) | (any(ev < tol))) {
      for ( i in 1:length(ev)) {
        ord.labels[i] <- paste("DIM", i, sep = "")
      }
    }
    else {
      ev.pc <- round(100*(ev/sum(ev)), 2)
      axis.names <- names(ev)
      if (is.null(axis.names)) {
        for ( i in 1:length(ev.pc)) {
          ord.labels[i] <- paste("DIM", i, " ", sprintf(ev.pc[i], fmt = '%#.1f'), "%", sep="")
        }
      } else {
        for (i in 1:length(ev.pc)){
          ord.labels[i] <- paste(axis.names[i], " ", ev.pc[i],"%", sep="")
        }
      }
    }
    return(ord.labels)
  }


