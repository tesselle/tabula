# Hurlbert =====================================================================
# Hurlbert rarefaction
#
# Hurlbert's unbiaised estimate of Sander's rarefaction.
# @param n A \code{\link{numeric}} vector giving the number of individuals for
#  each type.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Hurlbert, S. H. (1971). The Nonconcept of Species Diversity: A Critique and
#  Alternative Parameters. \emph{Ecology}, 52(4), 577-586.
#  DOI: \href{https://doi.org/10.2307/1934145}{10.2307/1934145}.
#
#  Sander, H. L. (1968). Marine Benthic Diversity: A Comparative Study.
#  \emph{The American Naturalist}, 102(925), 243-282.
# @author N. Frerebeau
# @family rarefaction index
# @rdname hurlbert-index
hurlbertRarefaction <- function(n, sample) {
  # Strictly positive whole numbers
  n <- trunc(n, digits = 0)[n > 0]
  sample <- trunc(sample, digits = 0)

  N <- sum(n)
  E <- sapply(X = n, FUN = function(x, N, sample) {
    if (N - x > sample)
      1 - combination(N - x, sample) / combination(N, sample)
    else
      NA
  }, N, sample)
  E <- sum(E)
  return(E)
}

# Margalef =====================================================================
# Margalef index
#
# @param n A \code{\link{numeric}} vector giving the number of individuals for
#  each type.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Margalef, R. (1958). Information Theory in Ecology. \emph{General Systems},
#  3, 36-71.
# @author N. Frerebeau
# @family richness index
# @rdname margalef-index
margalefRichness <- function(n, ...) {
  n <- n[n > 0] # Remove zeros
  N <- sum(n)
  S <- length(n)
  D <- (S - 1) / log(N, base = exp(1))
  return(D)
}

# Menhinick ====================================================================
# Menhinick diversity index
#
# @param n A \code{\link{numeric}} vector giving the number of individuals for
#  each type.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Menhinick, E. F. (1964). A Comparison of Some Species-Individuals Diversity
#  Indices Applied to Samples of Field Insects. \emph{Ecology}, 45(4), 859-861.
#  DOI: \href{https://doi.org/10.2307/1934933}{10.2307/1934933}.
# @author N. Frerebeau
# @family richness index
# @rdname menhinick-index
menhinickRichness <- function(n, ...) {
  n <- n[n > 0] # Remove zeros
  N <- sum(n)
  S <- length(n)
  D <- S / sqrt(N)
  return(D)
}
