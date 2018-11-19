#' @include AllGenerics.R AllClasses.R method-seriation.R statistics.R utilities.R
NULL

# Refine matrix seriation ======================================================
#' @export
#' @rdname seriation
#' @aliases refine,CountMatrix-method
setMethod(
  f = "refine",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2), ...) {
    # Partial bootstrap CA
    hull_rows <- bootCA(object, n = n, margin = 1, axes = axes, ...)
    hull_columns <- rows <- bootCA(object, n = n, margin = 2, axes = axes, ...)
    # Get convex hull maximal dimension length for each sample
    hull_length <- sapply(X = hull_rows, function(x) {
      max(stats::dist(x, method = "euclidean"))
    })
    # Get cutoff value
    limit <- cutoff(hull_length)
    # Samples to be kept
    keep <- which(hull_length < limit)
    # Bind hull vertices in a data.frame
    rows <- dplyr::bind_rows(hull_rows, .id = "id")
    cols <- dplyr::bind_rows(hull_columns, .id = "id")

    methods::new("BootCA",
                 rows = rows, columns = cols, cutoff = limit,
                 lengths = hull_length, keep = keep)
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
