#' @include AllGenerics.R AllClasses.R seriation.R
NULL

# Matrix seriation order =======================================================
#' @export
#' @rdname seriate
#' @aliases seriate,CountMatrix,missing-method
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix", subset = "missing"),
  definition = function(object, method = c("correspondance", "reciprocal"),
                        EPPM = FALSE, margin = c(1, 2), stop = 100, ...) {
    seriation(object, method = method, EPPM = EPPM, margin = margin,
              stop = stop, ...)
  }
)

#' @export
#' @rdname seriate
#' @aliases seriate,IncidenceMatrix,missing-method
setMethod(
  f = "seriate",
  signature = signature(object = "IncidenceMatrix", subset = "missing"),
  definition = function(object, method = c("correspondance", "reciprocal"),
                        margin = c(1, 2), stop = 100, ...) {
    seriation(object * 1, method = method, EPPM = FALSE, margin = margin,
              stop = stop, ...)
  }
)

#' @export
#' @rdname seriate
#' @aliases seriate,CountMatrix,BootCA-method
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix", subset = "BootCA"),
  definition = function(object, subset, margin = c(1, 2), ...) {
    # Validation
    margin <- as.integer(margin)

    # Original sequences
    m <- nrow(object)
    p <- ncol(object)
    i <- seq_len(m)
    j <- seq_len(p)

    # Correspondance analysis
    index_rows <- subset[["keep"]][[1]]
    index_columns <- subset[["keep"]][[2]]

    supp_rows <- supp_columns <- NULL
    if (length(index_rows) < m & 1 %in% margin) {
      supp_rows <- i[-index_rows]
    }
    if (length(index_columns) < p & 2 %in% margin) {
      supp_columns <- j[-index_columns]
    }

    corresp <- FactoMineR::CA(object, row.sup = supp_rows,
                              col.sup = supp_columns, graph = FALSE, ...)

    # Bind and reorder coords
    all_rows <- rbind(corresp$row$coord, corresp$row.sup$coord)
    ordered_rows <- all_rows[order(c(index_rows, supp_rows)), ]
    all_columns <- rbind(corresp$col$coord, corresp$col.sup$coord)
    ordered_columns <- all_columns[order(c(index_columns, supp_columns)), ]

    # Sequence of the first axis as best seriation order
    row_coords <- if (1 %in% margin) order(ordered_rows[, 1]) else i
    col_coords <- if (2 %in% margin) order(ordered_columns[, 1]) else j

    # New PermutationOrder object
    PermutationOrder(
      id = object[["id"]],
      rows = row_coords,
      columns = col_coords,
      method = "refined correspondance"
    )
  }
)

# Permute matrix ===============================================================
#' @export
#' @rdname seriate
#' @aliases permute,CountMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "CountMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Validation
    compareUUID(object[["id"]], order[["id"]])

    # Rearrange matrix
    new_matrix <- object[order@rows, order@columns]
    # New CountMatrix object
    .CountMatrix(new_matrix)
  }
)

#' @export
#' @rdname seriate
#' @aliases permute,IncidenceMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "IncidenceMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Validation
    compareUUID(object[["id"]], order[["id"]])

    # Rearrange matrix
    new_matrix <- object[order@rows, order@columns]
    # New CountMatrix object
    .IncidenceMatrix(new_matrix)
  }
)
