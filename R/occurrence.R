# CO-OCCURRENCE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname occurrence
#' @aliases occurrence,matrix-method
setMethod(
  f = "occurrence",
  signature = signature(object = "matrix"),
  definition = function(object) {
    incid <- object > 0
    m <- nrow(incid)
    p <- ncol(incid)

    ij <- utils::combn(p, m = 2, simplify = TRUE)
    pair <- seq_len(ncol(ij))

    mtx <- matrix(data = 0L, nrow = p, ncol = p)
    labels <- colnames(incid)
    dimnames(mtx) <- list(labels, labels)

    for (k in pair) {
      i <- ij[1, k]
      j <- ij[2, k]
      z <- as.integer(sum(incid[, i] + incid[, j] == 2))
      mtx[i, j] <- mtx[j, i] <- z
    }

    occ <- stats::as.dist(mtx)
    attr(occ, "total") <- m
    occ
  }
)

#' @export
#' @rdname occurrence
#' @aliases occurrence,data.frame-method
setMethod(
  f = "occurrence",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)
