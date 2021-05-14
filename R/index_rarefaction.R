#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname richness-index
#' @aliases rarefaction,CountMatrix-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "CountMatrix"),
  definition = function(object, sample, method = c("hurlbert"),
                        simplify = TRUE, ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_rarefaction(method) # Select method

    apply(X = object, MARGIN = 1, FUN = fun, sample)
  }
)

switch_rarefaction <- function(x) {
  switch (
    x,
    hurlbert = rarefactionHurlbert,
    stop(sprintf("There is no such method: %s.", x), call. = FALSE)
  )
}

# ==============================================================================
#' Rarefaction index
#'
#' Returns Hurlbert's unbiased estimate of Sander's rarefaction.
#' @param n A [`numeric`] vector giving the number of individuals for
#'  each type.
#' @param ... Currently not used.
#' @return A length-one [`numeric`] vector.
#' @author N. Frerebeau
#' @family diversity measures
#' @name index-rarefaction
#' @keywords internal
#' @noRd
rarefactionHurlbert <- function(x, sample) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (!is.numeric(sample) || length(sample) != 1)
    stop("`sample` must be a numeric scalar.")
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
