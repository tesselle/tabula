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
#' \code{rarefactionHurlbert} returns Hurlbert's unbiased estimate of Sander's
#' rarefaction.
#' @param n A \code{\link{numeric}} vector giving the number of individuals for
#'  each type.
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @references
#'  Hurlbert, S. H. (1971). The Nonconcept of Species Diversity: A Critique and
#'  Alternative Parameters. \emph{Ecology}, 52(4), 577-586.
#'  DOI: \href{https://doi.org/10.2307/1934145}{10.2307/1934145}.
#'
#'  Sander, H. L. (1968). Marine Benthic Diversity: A Comparative Study.
#'  \emph{The American Naturalist}, 102(925), 243-282.
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
