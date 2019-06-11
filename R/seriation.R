# SERIATION METHODS

# Seriation methods
seriation <- function(object, method = c("correspondance", "reciprocal"),
                      EPPM = FALSE, axes = 1, margin = c(1, 2), stop = 100,
                      ...) {
  # Validation
  checkType(object, expected = "numeric")
  checkScalar(EPPM, expected = "logical")
  checkScalar(axes, expected = "numeric")
  checkType(margin, expected = "numeric")
  checkScalar(stop, expected = "numeric")
  method <- match.arg(method, several.ok = FALSE)
  data <- if(EPPM) independance(object, method = "EPPM") else object

  index <- switch(
    method,
    reciprocal = seriationReciprocal(data, margin = margin, stop = stop),
    correspondance = seriationCorrespondance(data, margin = margin,
                                             axes = axes, ...),
    stop(sprintf("There is no such method: %s.", method), call. = FALSE)
  )

  # New PermutationOrder object
  PermutationOrder(
    id = object[["id"]],
    rows = as.integer(index[[1]]),
    columns = as.integer(index[[2]]),
    method = method
  )
}

# ==============================================================================
#' Probabilistic seriation methods
#'
#' \code{seriationReciprocal} computes reciprocal ranking/averaging.
#' \code{seriationCorrespondance} computes CA-based seriation.
#' @param x A \code{\link{numeric}} matrix.
#' @param stop A length-one \code{\link{numeric}} vector giving the stopping rule
#'  (i.e. maximum number of iterations) to avoid infinite loop.
#' @param margin A \code{\link{numeric}} vector giving the subscripts which the
#'  rearrangement will be applied over. E.g., for a matrix \code{1} indicates
#'  rows, \code{2} indicates columns, \code{c(1, 2)} indicates rows then columns,
#'  \code{c(2, 1)} indicates columns then rows.
#' @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
#' @return A list of two \code{\link{numeric}} vectors.
#' @author N. Frerebeau
#' @family seriation methods
#' @name seriation-probabilistic
#' @keywords internal
NULL

#' @rdname seriation-probabilistic
seriationReciprocal <- function(x, margin = 1, stop = 100) {
  # Validation
  margin <- as.integer(margin)
  stop <- as.integer(stop)

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
  index <- list(rows = seq_len(nrow(x)), columns = seq_len(ncol(x)))
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
      warning("Convergence not reached (possible infinite cycle).",
              call. = FALSE)
      break
    }
  }

  index
}

#' @rdname seriation-probabilistic
seriationCorrespondance <- function(x, margin, axes = 1, ...) {
  # Validation
  margin <- as.integer(margin)
  axes <- as.integer(axes)[[1L]]

  # Original sequences
  i <- seq_len(nrow(x))
  j <- seq_len(ncol(x))
  # Correspondance analysis
  corresp <- ca::ca(x)
  # Sequence of the first axis as best seriation order
  coords <- ca::cacoord(corresp, type = "principal")
  row_coords <- if (1 %in% margin) order(coords$rows[, axes]) else i
  col_coords <- if (2 %in% margin) order(coords$columns[, axes]) else j

  list(rows = row_coords, columns = col_coords)
}
