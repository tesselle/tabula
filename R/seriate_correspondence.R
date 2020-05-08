#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,CountMatrix,missing-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "CountMatrix", subset = "missing"),
  definition = function(object, margin = c(1, 2), ...) {
    seriation_correspondence(object, margin = margin, ...)
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,IncidenceMatrix-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "IncidenceMatrix", subset = "missing"),
  definition = function(object, margin = c(1, 2), ...) {
    seriation_correspondence(object, margin = margin, ...)
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,CountMatrix,BootCA-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "CountMatrix", subset = "BootCA"),
  definition = function(object, subset, margin = c(1, 2), ...) {
    # Validation
    margin <- as.integer(margin)
    # Get data
    counts <- arkhe::as_matrix(object)

    # Original sequences
    m <- nrow(counts)
    p <- ncol(counts)
    i <- seq_len(m)
    j <- seq_len(p)

    # correspondence analysis
    index_rows <- subset[["keep"]][[1]]
    index_columns <- subset[["keep"]][[2]]

    supp_rows <- supp_columns <- NA
    if (length(index_rows) < m & 1 %in% margin) {
      supp_rows <- i[-index_rows]
    }
    if (length(index_columns) < p & 2 %in% margin) {
      supp_columns <- j[-index_columns]
    }

    # correspondence analysis
    corresp <- ca::ca(counts, suprow = supp_rows, supcol = supp_columns, ...)
    # Sequence of the first axis as best seriation order
    coords <- ca::cacoord(corresp, type = "principal")
    row_coords <- if (1 %in% margin) order(coords$rows[, 1]) else i
    col_coords <- if (2 %in% margin) order(coords$columns[, 2]) else j

    # New PermutationOrder object
    .PermutationOrder(
      id = arkhe::get_id(object),
      rows = row_coords,
      columns = col_coords,
      method = "refined correspondence"
    )
  }
)

#' CA-based Seriation
#'
#' Computes CA-based seriation.
#' @param x A \linkS4class{DataMatrix} object.
#' @param margin A \code{\link{numeric}} vector giving the subscripts which the
#'  rearrangement will be applied over. E.g., for a matrix \code{1} indicates
#'  rows, \code{2} indicates columns, \code{c(1, 2)} indicates rows then columns,
#'  \code{c(2, 1)} indicates columns then rows.
#' @param ... Further arguments to be passed to \code{\link[ca]{ca}}.
#' @return A \linkS4class{PermutationOrder} object.
#' @author N. Frerebeau
#' @family seriation methods
#' @keywords internal
#' @noRd
seriation_correspondence <- function(x, margin, axes = 1, ...) {
  # Validation
  margin <- as.integer(margin)
  axes <- as.integer(axes)[[1L]]

  # Original sequences
  i <- seq_len(nrow(x))
  j <- seq_len(ncol(x))
  # Correspondence analysis
  corresp <- run_ca(x, ...)
  # Sequence of the first axis as best seriation order
  coords <- list(rows = corresp@row_coordinates,
                 columns = corresp@column_coordinates)
  row_coords <- if (1 %in% margin) order(coords$rows[, axes]) else i
  col_coords <- if (2 %in% margin) order(coords$columns[, axes]) else j

  # New PermutationOrder object
  .PermutationOrder(
    id = arkhe::get_id(x),
    rows = as.integer(row_coords),
    columns = as.integer(col_coords),
    method = "correspondance"
  )
}
