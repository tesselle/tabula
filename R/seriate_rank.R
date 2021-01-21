#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases seriate_rank,CountMatrix-method
setMethod(
  f = "seriate_rank",
  signature = signature(object = "CountMatrix"),
  definition = function(object, EPPM = FALSE, margin = c(1, 2), stop = 100) {
    seriate_rank2(object, margin = margin, stop = stop, EPPM = EPPM)
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_rank,IncidenceMatrix-method
setMethod(
  f = "seriate_rank",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, margin = c(1, 2), stop = 100) {
    seriate_rank2(object, margin = margin, stop = stop, EPPM = FALSE)
  }
)

#' Reciprocal Ranking
#'
#' Computes reciprocal ranking seriation.
#' @param x A \linkS4class{DataMatrix} object.
#' @param stop A length-one \code{\link{numeric}} vector giving the stopping rule
#'  (i.e. maximum number of iterations) to avoid infinite loop.
#' @param margin A \code{\link{numeric}} vector giving the subscripts which the
#'  rearrangement will be applied over. E.g., for a matrix \code{1} indicates
#'  rows, \code{2} indicates columns, \code{c(1, 2)} indicates rows then columns,
#'  \code{c(2, 1)} indicates columns then rows.
#' @return A \linkS4class{PermutationOrder} object.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
seriate_rank2 <- function(x, margin = 1, stop = 100, EPPM = FALSE) {
  # Validation
  margin <- as.integer(margin)
  stop <- as.integer(stop)

  data <- as.matrix(x)
  if (EPPM) data <- eppm(x)

  # Compute ranks
  # margin = 1 : on rows
  # margin = 2 : on columns
  reorder <- function(x, margin) {
    i <- seq_len(nrow(x))
    j <- seq_len(ncol(x))
    k <- switch(
      margin,
      `1` = colSums(t(x) * j) / rowSums(x),
      `2` = colSums(x * i) / colSums(x),
      stop("`margin` subscript out of bounds.", call. = FALSE)
    )
    order(k)
  }

  start <- 0
  index <- list(rows = seq_len(nrow(data)), columns = seq_len(ncol(data)))
  convergence <- FALSE
  while (!convergence) {
    old_index <- index
    # Rearrange along margins
    for (k in margin) {
      index[[k]] <- index[[k]][reorder(data[index[[1]], index[[2]]], margin = k)]
    }
    # Loop counter
    convergence <- identical(index, old_index)
    start <- start + 1
    if (start >= stop) {
      warning("Convergence not reached (possible infinite cycle).",
              call. = FALSE)
      break
    }
  }

  # New PermutationOrder object
  .PermutationOrder(
    rows = as.integer(index[[1]]),
    columns = as.integer(index[[2]]),
    method = "reciprocal ranking"
  )
}
