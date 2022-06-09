#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname rarefaction
#' @aliases rarefaction,matrix-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "matrix"),
  definition = function(object, sample, method = c("hurlbert")) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    apply(X = object, MARGIN = 1, FUN = fun, sample)
  }
)

#' @export
#' @rdname rarefaction
#' @aliases rarefaction,data.frame-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "data.frame"),
  definition = function(object, sample, method = c("hurlbert")) {
    object <- data.matrix(object)
    methods::callGeneric(object, sample = sample, method = method)
  }
)

# Index ========================================================================
#' @export
#' @rdname rarefaction
#' @aliases index_hurlbert,numeric-method
setMethod(
  f = "index_hurlbert",
  signature = signature(x = "numeric"),
  definition = function(x, sample, ...) {
    # Strictly positive whole numbers
    x <- trunc(x, digits = 0)[x > 0]
    sample <- trunc(sample, digits = 0)

    N <- sum(x)
    E <- vapply(
      X = x,
      FUN = function(x, N, sample) {
        if (N - x > sample) {
          1 - combination(N - x, sample) / combination(N, sample)
        } else {
          NA
        }
      },
      FUN.VALUE = double(1),
      N, sample
    )
    E <- sum(E)
    return(E)
  }
)
