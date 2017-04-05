#' Return matrix go.
#'
#' This is a copy of vegan::ordicluster except with the plotting
#' statement commented out.Not exported.
#'
# Like the original, it returns silently a matrix with columns go[ , 1], go[ , 2], w
#' @param ord An ordinaiton object.
#' @param clstr.rslt REsult from hclust based on same distance as ordination.
#' @param prune 0; do not change.
#' @param display "sites," do not change.
#' @param w weights function; do not change.
#' @param col 1; do not change.
#'
#' @importFrom grDevices col2rgb
#' @importFrom stats weighted.mean
#' @importFrom stats weights
#'
#' @return Matrix used by gg_ordicluster to compute line segments.
#'
my.ordicluster <- function (ord, clstr.rslt, prune = 0, display = "sites", w =weights(ord, display), col = 1)
{
  weights.default <- function(object, ...) NULL
  w <- eval(w)
  mrg <- clstr.rslt$merge
  my.col <- rep(NA, nrow(mrg))
  ord <- scores(ord, display = display, choices=c(1,2))
  if (nrow(mrg) != nrow(ord) - 1)
    stop("Dimensions do not match in 'ord' and 'cluster'")
  if (length(w) == 1)
    w <- rep(w, nrow(ord))
  n <- if (is.null(w))
    rep(1, nrow(ord))
  else w
  noden <- numeric(nrow(mrg) - prune)
  go <- matrix(0, nrow(mrg) - prune, 2)
  col <- rep(col, length = nrow(ord))
  col <- col2rgb(col)/255
  nodecol <- matrix(NA, nrow(mrg) - prune, 3)
  for (i in 1:(nrow(mrg) - prune)) {
    a <- mrg[i, 1]
    b <- mrg[i, 2]
    one <- if (a < 0)
      ord[-a, ]
    else go[a, ]
    two <- if (b < 0)
      ord[-b, ]
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

    #ordiArgAbsorber(one[1], one[2], two[1], two[2], col = rgb(t(nodecol[i, ])), FUN = segments, ...)
  }
  invisible(cbind(go, w))
}
