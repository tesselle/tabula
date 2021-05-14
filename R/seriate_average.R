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
#' @param object A [`CountMatrix-class`] or [`IncidenceMatrix-class`]
#'  object.
#' @param margin A [`numeric`] vector giving the subscripts which the
#'  rearrangement will be applied over: `1` indicates rows, `2`
#'  indicates columns, `c(1, 2)` indicates rows then columns, `c(2, 1)`
#'  indicates columns then rows.
#' @param axes An [`integer`] giving the CA dimension to be used for
#'  permutation.
#' @param ... Extra arguments to be passed to [dimensio::ca()].
#' @return A [`PermutationOrder-class`] object.
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
