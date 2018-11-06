#' @include AllGenerics.R AllClasses.R method-seriation.R
NULL

# Matrix seriation order =======================================================
#' @export
#' @rdname seriation
#' @aliases seriate,CountMatrix-method
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("ranking", "correspondance"),
                        EPPM = FALSE, margin = c(1, 2), stop = 100, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    data <- if (EPPM) threshold(object, method = "EPPM") else object

    index <- switch (
      method,
      ranking = reciprocalRanking(data, margin, stop),
      correspondance = reciprocalAveraging(data, ...)
    )
    # Coerce indices to integer
    index <- lapply(X = index, FUN = as.integer)
    # New PermutationOrder object
    methods::new("PermutationOrder", rows = index[[1]], columns = index[[2]],
                 seriation = method)
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
