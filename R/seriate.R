#' @include AllGenerics.R AllClasses.R method-seriation.R refine.R statistics.R utilities.R
NULL

# Matrix seriation order =======================================================
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

#' @export
#' @rdname seriation
#' @aliases seriate,CountMatrix,missing-method
setMethod(
  f = "seriate",
  signature = signature("CountMatrix", "missing"),
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
#' @aliases seriate,CountMatrix,BootCA-method
setMethod(
  f = "seriate",
  signature = signature("CountMatrix", "BootCA"),
  definition = function(object, constraint, margin = c(1, 2), ...) {
    # Validation
    margin <- as.integer(margin)

    # Original sequences
    i <- 1:nrow(object)
    j <- 1:ncol(object)

    # Correspondance analysis
    index <- constraint[["keep"]]
    suppl <- i[-index]
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
                 method = "correspondance")
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate,IncidenceMatrix,missing-method
setMethod(
  f = "seriate",
  signature = signature("IncidenceMatrix", "missing"),
  definition = function(object, method = c("correspondance", "reciprocal"),
                        margin = c(1, 2), stop = 100, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    # Seriation
    seriation(object * 1, method = method, EPPM = FALSE, margin = margin,
              stop = stop, ...)
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
