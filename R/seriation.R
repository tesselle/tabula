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

    return(index)
  }
)

# Permute matrix ===============================================================
#' @export
#' @rdname seriation
#' @aliases permute,CountMatrix-method
setMethod(
  f = "permute",
  signature = signature(object = "CountMatrix"),
  definition = function(object, order) {
    # Permutation order
    row_order <- order[[1]]
    col_order <- order[[2]]
    # Rearrange matrix
    new_matrix <- object[row_order, col_order]
    # New CountMatrix object
    new("CountMatrix", new_matrix)
  }
)
