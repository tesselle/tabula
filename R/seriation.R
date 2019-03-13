# SERIATION METHODS

# Seriation methods
#' @keywords internal
#' @noRd
seriation <- function(object, method = c("correspondance", "reciprocal"),
                      EPPM = FALSE, axes = 1, margin = c(1, 2), stop = 100,
                      ...) {

  data <- if (EPPM) independance(object, method = "EPPM") else object

  index <- switch (
    method,
    reciprocal = reciprocalSeriation(data, margin = margin, stop = stop),
    correspondance = correspondanceSeriation(data, margin = margin,
                                             axes = axes, ...),
    stop(paste("there is no such method:", method, sep = " "))
  )
  # Coerce indices to integer
  index <- lapply(X = index, FUN = as.integer)
  # New PermutationOrder object
  methods::new("PermutationOrder",
               rows = index[[1]], columns = index[[2]], method = method)
}

# Probabilistic methods ========================================================
#' Reciprocal ranking/averaging
#'
#' @param x A \code{\link{numeric}} matrix.
#' @param stop A length-one \code{\link{numeric}} vector giving the stopping rule
#'  (i.e. maximum number of iterations) to avoid infinite loop.
#' @param margin A \code{\link{numeric}} vector giving the subscripts which the
#'  rearrangement will be applied over. E.g., for a matrix \code{1} indicates
#'  rows, \code{2} indicates columns, \code{c(1, 2)} indicates rows then columns,
#'  \code{c(2, 1)} indicates columns then rows.
#' @return A list of two \code{\link{numeric}} vectors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
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

#' CA-based seriation
#'
#' @param x A \code{\link{numeric}} matrix.
#' @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
#' @return A list of two \code{\link{numeric}} vectors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
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
