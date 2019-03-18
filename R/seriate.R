#' @include AllGenerics.R AllClasses.R seriation.R
NULL

# Matrix seriation order =======================================================
#' @export
#' @rdname seriation
#' @aliases seriate,CountMatrix,missing-method
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix", subset = "missing"),
  definition = function(object, method = c("correspondance", "reciprocal"),
                        EPPM = FALSE, margin = c(1, 2), stop = 100, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    # Seriation
    seriation(object, method = method, EPPM = EPPM, margin = margin,
              stop = stop, ...)
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate,IncidenceMatrix,missing-method
setMethod(
  f = "seriate",
  signature = signature(object = "IncidenceMatrix", subset = "missing"),
  definition = function(object, method = c("correspondance", "reciprocal"),
                        margin = c(1, 2), stop = 100, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    # Seriation
    seriation(object * 1, method = method, EPPM = FALSE, margin = margin,
              stop = stop, ...)
  }
)

#' @export
#' @rdname seriation
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
    i <- 1:m
    j <- 1:p

    # Correspondance analysis
    index <- subset[["keep"]]
    suppl <- if (length(index) < m) i[-index] else NULL

    corresp <- FactoMineR::CA(object, row.sup = suppl, graph = FALSE, ...)

    # Bind and reorder coords
    all_coords <- rbind(corresp$row$coord, corresp$row.sup$coord)
    ordered_coords <- all_coords[order(c(index, suppl)), ]

    # Sequence of the first axis as best seriation order
    row_coords <- if (1 %in% margin) order(ordered_coords[, 1]) else i
    col_coords <- if (2 %in% margin) order(corresp$col$coord[, 1]) else j

    # New PermutationOrder object
    methods::new("PermutationOrder",
                 rows = as.integer(row_coords),
                 columns = as.integer(col_coords),
                 method = "refined correspondance")
  }
)

# Permute matrix ===============================================================
#' @export
#' @rdname seriation
#' @aliases permute,CountMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "CountMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange matrix
    new_matrix <- object[order@rows, order@columns]
    # New CountMatrix object
    methods::new("CountMatrix", new_matrix)
  }
)

#' @export
#' @rdname seriation
#' @aliases permute,IncidenceMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "IncidenceMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange matrix
    new_matrix <- object[order@rows, order@columns]
    # New CountMatrix object
    methods::new("IncidenceMatrix", new_matrix)
  }
)
