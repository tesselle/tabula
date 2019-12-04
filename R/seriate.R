#' @include AllGenerics.R AllClasses.R seriation.R
NULL

# Matrix seriation order =======================================================
## Reciprocal seriation --------------------------------------------------------
#' @export
#' @rdname seriation
#' @aliases seriate_reciprocal,AbsoluteFrequencyMatrix-method
setMethod(
  f = "seriate_reciprocal",
  signature = signature(object = "AbsoluteFrequencyMatrix"),
  definition = function(object, EPPM = FALSE, margin = c(1, 2), stop = 100) {

    seriation(object, method = "reciprocal", EPPM = EPPM, margin = margin,
              stop = stop)

  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_reciprocal,IncidenceMatrix-method
setMethod(
  f = "seriate_reciprocal",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, margin = c(1, 2), stop = 100) {

    seriation(object, method = "reciprocal", margin = margin, stop = stop)

  }
)

## CA seriation ----------------------------------------------------------------
#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,AbsoluteFrequencyMatrix,missing-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "AbsoluteFrequencyMatrix", subset = "missing"),
  definition = function(object, margin = c(1, 2), ...) {

    seriation(object, method = "correspondence", margin = margin, ...)

  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,IncidenceMatrix-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "IncidenceMatrix", subset = "missing"),
  definition = function(object, margin = c(1, 2), ...) {

    seriation(object, method = "correspondence", margin = margin, ...)

  }
)

## CA refined seriation --------------------------------------------------------
#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,AbsoluteFrequencyMatrix,BootCA-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "AbsoluteFrequencyMatrix", subset = "BootCA"),
  definition = function(object, subset, margin = c(1, 2), ...) {
    # Validation
    margin <- as.integer(margin)

    # Original sequences
    m <- nrow(object)
    p <- ncol(object)
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
    corresp <- ca::ca(object, suprow = supp_rows, supcol = supp_columns, ...)
    # Sequence of the first axis as best seriation order
    coords <- ca::cacoord(corresp, type = "principal")
    row_coords <- if (1 %in% margin) order(coords$rows[, 1]) else i
    col_coords <- if (2 %in% margin) order(coords$columns[, 2]) else j

    # New PermutationOrder object
    .PermutationOrder(
      id = object[["id"]],
      rows = row_coords,
      columns = col_coords,
      method = "refined correspondence"
    )
  }
)

# Permute matrix ===============================================================
#' @export
#' @rdname seriation
#' @aliases permute,AbsoluteFrequencyMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "AbsoluteFrequencyMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Validation
    compare_uuid(object[["id"]], order[["id"]])

    # Rearrange matrix
    new_matrix <- object[order[["rows"]], order[["columns"]]]
    # New AbsoluteFrequencyMatrix object
    .AbsoluteFrequencyMatrix(new_matrix, id = order[["id"]])
  }
)

#' @export
#' @rdname seriation
#' @aliases permute,IncidenceMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "IncidenceMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Validation
    compare_uuid(object[["id"]], order[["id"]])

    # Rearrange matrix
    new_matrix <- object[order[["rows"]], order[["columns"]]]
    # New AbsoluteFrequencyMatrix object
    .IncidenceMatrix(new_matrix, id = order[["id"]])
  }
)
