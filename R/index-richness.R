#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname richness-method
#' @aliases richness,CountMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("ace", "chao1", "chao1bc", "chao1i",
                                           "margalef", "menhinick"),
                        k = 10, simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- sapply(X = method, FUN = function(x, object, k) {
      index <- switch (
        x,
        ace = aceRichness,
        chao1 = chao1Richness,
        chao1bc = chao1bcRichness,
        chao1i = chao1iRichness,
        margalef = margalefRichness,
        menhinick = menhinickRichness
      )
      apply(X = object, MARGIN = 1, FUN = index, k)
    }, object, k, simplify = simplify)
    return(E)
  }
)

#' @export
#' @rdname richness-method
#' @aliases richness,IncidenceMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("chao2", "chao2i", "ice"), k = 10,
                        simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    index <- switch (
      method,
      ice = iceRichness,
      chao2 = chao2Richness,
      chao2i = chao2iRichness
    )
    E <- index(object, k)
    return(E)
  }
)

# Richness =====================================================================
# Abundance data ---------------------------------------------------------------

# Abundance-based Coverage Estimator (ACE)
#
# @param n A \code{\link{numeric}} vector giving the number of individuals for
#  each type.
# @param k A \code{\link{numeric}} vector giving the threshold between rare and
#  abundant species.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @references
#  Chao, A., & Lee, S.-M. (1992). Estimating the Number of Classes via Sample
#  Coverage. \emph{Journal of the American Statistical Association}, 87(417),
#  210-217.
#  DOI: \href{https://doi.org/10.1080/01621459.1992.10475194}{10.1080/01621459.1992.10475194}
# @family richness index
# @rdname ace-index
aceRichness <- function(n, k = 10, ...) {
  n <- n[n > 0] # Remove unobserved species

  S <- length(n) # Number of observed species
  S_rare <- sum(n <= k) # Number of rare species
  S_abun <- sum(n > k) # Number of abundant species

  N_rare <- sum(n[n <= k]) # Number of individuals in the rare species
  f1 <- sum(n == 1) # Number of singleton species
  if (f1 == N_rare)
    stop("ACE is undefined when all rare species are singletons,
         consider using the unbiased Chao1 index instead.")
  C_rare <- 1 - (f1 / N_rare) # Sample coverage estimate for rare species
  # ie. proportion of all individuals in rare species that are not singletons

  # Coefficient of variation
  a <- sum(sapply(X = 1:k, FUN = function(x, n) { x * (x - 1) * sum(n == x) }, n))
  b <- sum(sapply(X = 1:k, FUN = function(x, n) { x * sum(n == x) }, n))
  c <- sum(sapply(X = 1:k, FUN = function(x, n) { x * sum(n == x) - 1 }, n))
  g2 <- max((S_rare / C_rare) * (a / (b * c)) - 1, 0)

  D <- S_abun + S_rare / C_rare + f1 * g2 / C_rare
  return(D)
}

# Chao1 estimator
#
# Chao1 estimator for abundance data.
# @param n A \code{\link{numeric}} vector giving the number of individuals for
#  each type.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Chao, A. (1984). Nonparametric Estimation of the Number of Classes in a
#  Population. \emph{Scandinavian Journal of Statistics}, 11(4), 265-270.
# @author N. Frerebeau
# @family richness index
# @family Chao1 index
# @rdname chao1-index
chao1Richness <- function(n, ...) {
  n <- n[n > 0] # Remove unobserved species
  S <- length(n) # Number of observed species
  N <- sum(n) # Number of individuals
  f1 <- sum(n == 1) # Number of singleton species
  f2 <- sum(n == 2) # Number of doubleton species

  if (f2 == 0) {
    D <- S + ((N - 1) / N) * f1 * ((f1 - 1) / 2)
  } else {
    D <- S + ((N - 1) / N) * (f1^2 / (2 * f2))
  }

  return(D)
}

# Bias-corrected Chao1 estimator
#
# Bias-corrected Chao1 estimator for abundance data.
# @param n A \code{\link{numeric}} vector giving the number of individuals for
#  each type.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Chao, A. (1984). Nonparametric Estimation of the Number of Classes in a
#  Population. \emph{Scandinavian Journal of Statistics}, 11(4), 265-270.
# @author N. Frerebeau
# @family richness index
# @family Chao1 index
# @rdname bcchao1-index
chao1bcRichness <- function(n, ...) {
  n <- n[n > 0] # Remove unobserved species
  S <- length(n) # Number of observed species
  N <- sum(n) # Number of individuals
  f1 <- sum(n == 1) # Number of singleton species
  f2 <- sum(n == 2) # Number of doubleton species
  D <- S + (((N - 1) / N) * f1 * (f1 - 1)) / (2 * (f2 + 1))
  return(D)
}

# Improved Chao1 estimator
#
# Improved Chao1 estimator for abundance data.
# @param n A \code{\link{numeric}} vector giving the number of individuals for
#  each type.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Chiu, C.-H., Wang, Y.-T., Walther, B. A. & Chao, A. (2014). An improved
#  nonparametric lower bound of species richness via a modified good-turing
#  frequency formula. \emph{Biometrics}, 70(3), 671-682.
#  DOI: \href{https://doi.org/10.1111/biom.12200}{10.1111/biom.12200}.
# @author N. Frerebeau
# @family richness index
# @family Chao1 index
# @rdname ichao1-index
chao1iRichness <- function(n, ...) {
  n <- n[n > 0] # Remove unobserved species
  S <- length(n) # Number of observed species
  N <- sum(n) # Number of individuals
  f1 <- sum(n == 1) # Number of singleton species
  f2 <- sum(n == 2) # Number of doubleton species
  f3 <- sum(n == 3) # Number of tripleton species
  f4 <- sum(n == 4) # Number of quadrupleton species
  chao1 <- chao1Richness(n)
  k <- f1 - ((N - 3) / (N - 1)) * ((f2 * f3) / (2 * f4))
  D <- chao1 + ((N - 3) / N) * (f3 / (4 * f4)) * max(k, 0)
  return(D)
}

# Margalef richness
#
# Margalef richness index for abundance data.
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
  n <- n[n > 0] # Remove unobserved species
  N <- sum(n) # Number of individuals
  S <- length(n) # Number of observed species
  D <- (S - 1) / log(N, base = exp(1))
  return(D)
}

# Menhinick richness
#
# Menhinick richness index for abundance data.
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
  n <- n[n > 0] # Remove unobserved species
  N <- sum(n) # Number of individuals
  S <- length(n) # Number of observed species
  D <- S / sqrt(N)
  return(D)
}

# Incidence data ---------------------------------------------------------------
# Chao2 estimator
#
# Chao2 estimator for replicated incidence data.
# @param x A \code{\link{logical}} matrix.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Chao, A. (1987). Estimating the Population Size for Capture-Recapture Data
#  with Unequal Catchability. \emph{Biometrics} 43(4): 783.
#  DOI: \href{https://doi.org/10.2307/2531532}{10.2307/2531532}.
# @author N. Frerebeau
# @family richness index
# @family Chao2 index
# @rdname chao2-index
chao2Richness <- function(x, ...) {
  q <- colSums(x) # Number of species in the assemblage
  q <- q[q > 0] # Remove unobserved species
  S <- length(q) # Number of observed species
  t <- sum(rowSums(x) != 0) # Total number of samples
  q1 <- sum(q == 1) # Number of unique species in the assemblage
  q2 <- sum(q == 2) # Number of duplicate species in the assemblage

  if (q2 == 0) {
    D <- S + ((t - 1) / t) * q1 * ((q1 - 1) / 2)
  } else {
    D <- S + ((t - 1) / t) * (q1^2 / (2 * q2))
  }

  return(D)
}

# Bias-corrected Chao2 estimator
#
# Bias-corrected Chao2 estimator for replicated incidence data.
# @param x A \code{\link{logical}} matrix.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Chao, A. (1987). Estimating the Population Size for Capture-Recapture Data
#  with Unequal Catchability. \emph{Biometrics} 43(4): 783.
#  DOI: \href{https://doi.org/10.2307/2531532}{10.2307/2531532}.
# @author N. Frerebeau
# @family richness index
# @family Chao2 index
# @rdname chao2bc-index
chao2bcRichness <- function(x, ...) {
  q <- colSums(x) # Number of species in the assemblage
  q <- q[q > 0] # Remove unobserved species
  S <- length(q) # Number of observed species
  t <- sum(rowSums(x) != 0) # Total number of samples
  q1 <- sum(q == 1) # Number of unique species in the assemblage
  q2 <- sum(q == 2) # Number of duplicate species in the assemblage
  D <- S + ((t - 1) / t) * (q1 * (q1 - 1)) / (2 * (q2 + 1))
  return(D)
}

# Improved Chao2 estimator
#
# Improved Chao2 estimator for replicated incidence data.
# @param x A \code{\link{logical}} matrix.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Chiu, C.-H., Wang, Y.-T., Walther, B. A. & Chao, A. (2014). An improved
#  nonparametric lower bound of species richness via a modified good-turing
#  frequency formula. \emph{Biometrics}, 70(3), 671-682.
#  DOI: \href{https://doi.org/10.1111/biom.12200}{10.1111/biom.12200}.
# @author N. Frerebeau
# @family richness index
# @family Chao2 index
# @rdname chao2i-index
chao2iRichness <- function(x, ...) {
  q <- colSums(x) # Number of species in the assemblage
  q <- q[q > 0] # Remove unobserved species
  S <- length(q) # Number of observed species
  t <- sum(rowSums(x) != 0) # Total number of samples
  q1 <- sum(q == 1) # Number of unique species in the assemblage
  q2 <- sum(q == 2) # Number of duplicate species in the assemblage
  q3 <- sum(q == 3) # Number of tripleton species
  q4 <- sum(q == 4) # Number of quadrupleton species

  chao2 <- chao2Richness(q)
  k <- q1 - ((t - 3) / (t - 1)) * ((q2 * q3) / (2 * q4))
  D <- chao2 + ((t - 3) / (4 * t)) * (q3 / q4) * max(k, 0)
  return(D)
}

# Incidence-based Coverage Estimator (ICE)
#
# @param x A \code{\link{logical}} matrix.
# @param k A \code{\link{numeric}} vector giving the threshold between rare and
#  abundant species.
# @param ... Currently not used.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family richness index
# @rdname ice-index
iceRichness <- function(x, k, ...) {
  q <- colSums(x) # Number of species in the assemblage
  q <- q[q > 0] # Remove unobserved species

  S_infr <- sum(q <= k) # Number of infrequent species
  S_freq <- sum(q > k) # Number of frequent species
  N_infr <- sum(q[q <= k]) # Number of incidences in the infrequent species
  # Number of sampling units that include at least one infrequent species
  t <- sum(rowSums(x[, q <= k]) != 0)

  q1 <- sum(q == 1) # Number of unique species in the assemblage
  if (q1 == N_infr)
    stop("ICE is undefined when when all infrequent species are unique,
         consider using the unbiased Chao2 index instead.")
  C_infr <- 1 - (q1 / N_infr) # Sample coverage estimate
  # ie. proportion of all incidences of infrequent species that are not uniques

  # Coefficient of variation
  a <- sum(sapply(X = 1:k, FUN = function(x, q) { x * (x - 1) * sum(q == x) }, q))
  b <- sum(sapply(X = 1:k, FUN = function(x, q) { x * sum(q == x) }, q))
  c <- sum(sapply(X = 1:k, FUN = function(x, q) { x * sum(q == x) - 1 }, q))
  g2 <- max((S_infr / C_infr) * (t / (t - 1)) * (a / (b * c)) - 1, 0)

  D <- S_freq + S_infr / C_infr + q1 * g2 / C_infr
  return(D)
}
