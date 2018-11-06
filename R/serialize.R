# Reciprocal ranking
#
# @param x A \code{\link{numeric}} matrix.
# @param stop A length-one \code{\link{numeric}} vector giving the stopping rule
#  (i.e. maximum number of iterations) to avoid infinite loop.
# @param margin A \code{\link{numeric}} vector giving the subscripts which the
#  rearrangement will be applied over. E.g., for a matrix \code{1} indicates
#  rows, \code{2} indicates columns, \code{c(1, 2)} indicates rows then columns,
#  \code{c(2, 1)} indicates columns then rows.
# @return A list of two \code{\link{numeric}} vectors.
# @author N. Frerebeau
reciprocalRanking <- function(x, margin = 1, stop = 100) {
  # Validation
  if (length(margin) > 2)
    stop("`margin` should be one of `1`, `2`, `c(1, 2)` or `c(2, 1)`")
  # Compute ranks
  # margin = 1 : on rows
  # margin = 2 : on columns
  reorder <- function(x, margin) {
    i <- 1:nrow(x)
    j <- 1:ncol(x)
    k <- switch (margin,
      `1` = colSums(t(x) * j) / rowSums(x),
      `2` = colSums(x * i) / colSums(x)
    )
    order(k)
  }

  start <- 0
  index <- list(rows = 1:nrow(x), columns = 1:ncol(x))
  convergence <- FALSE
  while (!convergence) {
    old_index <- index
    # Rearrange along margins
    for (k in margin) {
      index[[k]] <- index[[k]][reorder(x[index[[1]], index[[2]]], margin = k)]
    }
    # Loop counter
    convergence <- identical(index, old_index)
    start <- start + 1
    if (start >= stop) {
      warning("convergence not reached (possible infinite cycle)")
      break
    }
  }

  return(index)
}

# Reciprocal averaging
#
# @param x A \code{\link{numeric}} matrix.
# @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
# @return A list of two \code{\link{numeric}} vectors.
# @author N. Frerebeau
reciprocalAveraging <- function(x, ...) {

  # Correspondance analysis
  corresp <- FactoMineR::CA(x, ..., graph = FALSE)
  # Sequence of the first axis as best seriation order
  row_coords <- order(corresp$row$coord[, 1])
  col_coords <- order(corresp$col$coord[, 1])

  return(list(rows = row_coords, columns = col_coords))
}
