#' @include AllGenerics.R AllClasses.R method-seriation.R statistics.R utilities.R
NULL

# Refine matrix seriation ======================================================
#' @export
#' @rdname seriation
#' @aliases refine,CountMatrix-method
setMethod(
  f = "refine",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, margin = 1,
                        axes = c(1, 2), ...) {
    # Partial bootstrap CA
    hull_area <- bootCA(object, n = n, margin = margin, axes = axes, ...)
    # Sample selection
    limit <- cutoff(hull_area)
    keep <- which(hull_area < limit)
    return(keep)
  }
)

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
#' @aliases seriate,CountMatrix-method
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix"),
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
#' @aliases seriate,IncidenceMatrix-method
setMethod(
  f = "seriate",
  signature = signature(object = "IncidenceMatrix"),
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
