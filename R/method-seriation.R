# Refine seriation =============================================================
# @param x An \eqn{m \times p}{m x p} data matrix.
# @param cutoff A function that takes a numeric vector as argument and returns
#  a single numeric value.
# @param margin A non-negative \code{\link{integer}} giving the subscripts
#  which the rearrangement will be applied over. E.g., for a matrix \code{1}
#  indicates rows, \code{2} indicates columns.
# @param n A non-negative \code{\link{integer}} giving the number of partial
#  bootstrap replications.
# @param axes A \code{\link{numeric}} vector giving the subscripts of the CA
#  components to use.
bootCA <- function(x, margin = 1, n = 1000, axes = c(1, 2), ...) {
  # Validation
  margin <- as.integer(margin)
  n <- as.integer(n)
  axes <- as.integer(axes)

  # CA on the whole dataset
  results_CA <- FactoMineR::CA(x, ..., graph = FALSE)
  svd <- if (margin == 1) results_CA$svd$V else results_CA$svd$U

  # Compute convex hull area for each replicated sample
  hull <- apply(
    X = x, MARGIN = margin,
    FUN = function(x, n, svd, axes) {
      # n random replicates
      replicated <- stats::rmultinom(n = n, size = sum(x), prob = x)
      # Compute new CA coordinates
      coords <- crossprod(replicated / colSums(replicated), svd)

      # Get convex hull coordinates
      points <- grDevices::chull(coords[, axes])
      # Repeat first point
      hull <- coords[c(points, points[1]), axes]
      colnames(hull) <- c("x", "y")

      return(as.data.frame(hull))
    }, n = n, svd = svd, axes = axes)

  return(hull)
}

# Seriation methods ============================================================
# Probabilistic methods --------------------------------------------------------
# Reciprocal ranking/averaging
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
reciprocalSeriation <- function(x, margin = 1, stop = 100) {
  # Validation
  margin <- as.integer(margin)
  stop <- as.integer(stop)

  # Compute ranks
  # margin = 1 : on rows
  # margin = 2 : on columns
  reorder <- function(x, margin) {
    i <- 1:nrow(x)
    j <- 1:ncol(x)
    k <- switch (margin,
      `1` = colSums(t(x) * j) / rowSums(x),
      `2` = colSums(x * i) / colSums(x),
      stop("'margin' subscript out of bounds")
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

# CA-based seriation
#
# @param x A \code{\link{numeric}} matrix.
# @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
# @return A list of two \code{\link{numeric}} vectors.
# @author N. Frerebeau
correspondanceSeriation <- function(x, margin, axes, ...) {
  # Validation
  margin <- as.integer(margin)
  axes <- as.integer(axes)

  # Original sequences
  i <- 1:nrow(x)
  j <- 1:ncol(x)
  # Correspondance analysis
  corresp <- FactoMineR::CA(x, ..., graph = FALSE)
  # Sequence of the first axis as best seriation order
  row_coords <- if (1 %in% margin) order(corresp$row$coord[, axes]) else i
  col_coords <- if (2 %in% margin) order(corresp$col$coord[, axes]) else j

  return(list(rows = row_coords, columns = col_coords))
}
