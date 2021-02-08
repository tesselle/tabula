#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases seriate_average,CountMatrix-method
setMethod(
  f = "seriate_average",
  signature = signature(object = "CountMatrix"),
  definition = function(object, margin = c(1, 2), axes = 1, ...) {
    seriate_average2(object = object, margin = margin, axes = axes, ...)
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_average-method
setMethod(
  f = "seriate_average",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, margin = c(1, 2), axes = 1, ...) {
    seriate_average2(object = object, margin = margin, axes = axes, ...)
  }
)

#' Average Ranking
#'
#' Computes average ranking (CA) seriation.
#' @param object A \linkS4class{CountMatrix} or \linkS4class{IncidenceMatrix}
#'  object.
#' @param margin A \code{\link{numeric}} vector giving the subscripts which the
#'  rearrangement will be applied over: \code{1} indicates rows, \code{2}
#'  indicates columns, \code{c(1, 2)} indicates rows then columns,
#'  \code{c(2, 1)} indicates columns then rows.
#' @param axes An \code{\link{integer}} giving the CA dimension to be used for
#'  permutation.
#' @param ... Extra arguments to be passed to \code{\link[arkhe]{ca}}.
#' @return A \linkS4class{PermutationOrder} object.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
seriate_average2 <- function(object, margin = c(1, 2), axes = 1, ...) {
  # Validation
  margin <- as.integer(margin)
  axes <- as.integer(axes)[[1L]]

  # Original sequences
  i <- seq_len(nrow(object))
  j <- seq_len(ncol(object))
  # Correspondence analysis
  corresp <- dimensio::ca(object, ...)
  # Sequence of the first axis as best seriation order
  coords <- list(
    rows = dimensio::get_coordinates(corresp, margin = 1),
    columns = dimensio::get_coordinates(corresp, margin = 2)
  )
  row_coords <- if (1 %in% margin) order(coords$rows[, axes]) else i
  col_coords <- if (2 %in% margin) order(coords$columns[, axes]) else j

  # New PermutationOrder object
  .PermutationOrder(
    rows = as.integer(row_coords),
    columns = as.integer(col_coords),
    method = "average ranking"
  )
}
