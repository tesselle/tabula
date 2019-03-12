#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname richness-method
#' @aliases rarefaction,CountMatrix-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "CountMatrix"),
  definition = function(object, sample) {
    E <- apply(X = object, MARGIN = 1, FUN = hurlbertRarefaction, sample)
    return(E)
  }
)

# Rarefaction ==================================================================
#' Hurlbert rarefaction
#'
#' Hurlbert's unbiaised estimate of Sander's rarefaction.
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
#' @family rarefaction index
#' @rdname hurlbert-index
#' @noRd
hurlbertRarefaction <- function(x, sample) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  # Strictly positive whole numbers
  x <- trunc(x, digits = 0)[x > 0]
  sample <- trunc(sample, digits = 0)

  N <- sum(x)
  E <- sapply(X = x, FUN = function(x, N, sample) {
    if (N - x > sample)
      1 - combination(N - x, sample) / combination(N, sample)
    else
      NA
  }, N, sample)
  E <- sum(E)
  return(E)
}
