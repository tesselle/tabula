#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname diversity-method
#' @aliases diversity,CountMatrix-method
setMethod(
  f = "diversity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        simplify = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    H <- sapply(X = method, FUN = function(x, data) {
      index <- switch (
        x,
        berger = bergerDominance,
        brillouin = brillouinDiversity,
        mcintosh = mcintoshDominance,
        shannon = shannonDiversity,
        simpson = simpsonDominance
      )
      apply(X = object, MARGIN = 1, FUN = index)
    }, data = object, simplify = simplify)
    return(H)
  }
)

#' @export
#' @rdname diversity-method
#' @aliases evenness,CountMatrix-method
setMethod(
  f = "evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        simplify = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- sapply(X = method, FUN = function(x, data) {
      index <- switch (
        x,
        brillouin = brillouinEvenness,
        mcintosh = mcintoshEvenness,
        shannon = shannonEvenness,
        simpson = simpsonEvenness
      )
      apply(X = object, MARGIN = 1, FUN = index)
    }, data = object, simplify = simplify)
    return(E)
  }
)

# Berger-Parker ================================================================
#' Berger-Parker dominance index
#'
#' @param x A \code{\link{numeric}} vector giving the number of individuals for
#'  each class.
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @references
#'  Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera
#'  in Deep-Sea Sediments. \emph{Science}, 168(3937), 1345-1347.
#'  DOI: \href{https://doi.org/10.1126/science.168.3937.1345}{10.1126/science.168.3937.1345}.
#' @author N. Frerebeau
#' @family diversity index
#' @rdname berger-index
#' @noRd
bergerDominance <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  Nmax <- max(x)
  N <- sum(x)
  d <- Nmax / N
  return(d)
}

# Brillouin ====================================================================
#' Brillouin diversity index and evenness
#'
#' @param x A \code{\link{numeric}} vector giving the number of individuals for
#'  each class.
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family diversity index
#' @rdname brillouin-index
#' @noRd
brillouinDiversity <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  HB <- (lfactorial(N) - sum(lfactorial(x))) / N
  return(HB)
}
#' @family evenness index
#' @rdname brillouin-index
#' @noRd
brillouinEvenness <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  HB <- brillouinDiversity(x)
  N <- sum(x)
  S <- length(x) # richness = number of different species
  a <- trunc(N / S)
  r <- N - S * a
  c <- (S - r) * lfactorial(a) + r * lfactorial(a + 1)
  HBmax <- (1 / N) * (lfactorial(N) - c)
  E <- HB / HBmax
  return(E)
}

# McIntosh =====================================================================
#' McIntosh dominance index and evenness
#'
#' @param x A \code{\link{numeric}} vector giving the number of individuals for
#'  each class.
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @references
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. \emph{Ecology}, 48(3), 392-404.
#'  DOI: \href{https://doi.org/10.2307/1932674}{10.2307/1932674}.
#' @author N. Frerebeau
#' @family diversity index
#' @rdname mcintosh-index
#' @noRd
mcintoshDominance <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  U <- sqrt(sum(x^2))
  D <- (N - U) / (N - sqrt(N))
  return(D)
}
#' @family evenness index
#' @rdname mcintosh-index
#' @noRd
mcintoshEvenness <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  S <- length(x) # richness = number of different species
  U <- sqrt(sum(x^2))
  E <- (N - U) / (N - (N / sqrt(S)))
  return(E)
}

# Shannon ======================================================================
#' Shannon-Wiener diversity index, evenness and variance
#'
#' @param x A \code{\link{numeric}} vector giving the number of individuals for
#'  each class.
#' @param base A positive or complex number: the base with respect to which
#'  logarithms are computed (see \code{\link[base]{log}}). Change this only if
#'  you know what you are doing!
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @references
#'  Shannon, C. E. (1948). A Mathematical Theory of Communication. \emph{The
#'  Bell System Technical Journal}, 27, 379-423.
#'  DOI: \href{https://doi.org/10.1002/j.1538-7305.1948.tb01338.x}{10.1002/j.1538-7305.1948.tb01338.x}.
#' @author N. Frerebeau
#' @family diversity index
#' @rdname shannon-index
#' @noRd
shannonDiversity <- function(x, base = exp(1), ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  S <- length(x) # richness = number of different species
  p <- x / N
  H <- -sum(p * log(p, base))
  # H <- H - (S - 1) / N +
  #   (1 - sum(p^-1)) / (12 * N^2) +
  #   (sum(p^-1 - p^-2)) / (12 * N^3)
  return(H)
}
#' @family evenness index
#' @rdname shannon-index
#' @noRd
shannonEvenness <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  E <- shannonDiversity(x, base = length(x))
  return(E)
}
#' @rdname shannon-index
#' @noRd
shannonVariance <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  S <- length(x) # richness = number of different species
  p <- x / N
  a <- sum(p * (log(p, base = exp(1)))^2)
  b <- sum(p * log(p, base = exp(1)))^2
  var <- ((a - b) / N) + ((S - 1) / (2 * N^2))
  return(var)
}

# Simpson ======================================================================
#' Simpson dominance index and evenness
#'
#' @param x A \code{\link{numeric}} vector giving the number of individuals for
#'  each class.
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @references
#'  Simpson, E. H. (1949). Measurement of Diversity. \emph{Nature}, 163(4148),
#'  688-688. DOI: \href{https://doi.org/10.1038/163688a0}{10.1038/163688a0}.
#' @author N. Frerebeau
#' @family diversity index
#' @rdname simpson-index
#' @noRd
simpsonDominance <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  D <- sum(x * (x - 1)) / (N* (N - 1)) # For discrete data
  return(D)
}
#' @family evenness index
#' @rdname simpson-index
#' @noRd
simpsonEvenness <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("numeric values expected")
  # Remove zeros
  x <- x[x > 0]

  D <- 1 / simpsonDominance(x)
  S <- length(x[x > 0]) # richness = number of different species
  E <- D / S
  return(E)
}

# Chao =========================================================================
# Chao index
#
# @param n A \code{\link{numeric}} vector.
# @param method A \code{\link{character}} string specifiying the method to be
#  used. This must be one of "MLEU", "CHAO" (see details). Any unambiguous
#  substring can be given.
# @param k A length-one \code{\link{numeric}} vector.
# @param base A positive or complex number: the base with respect to which
#  logarithms are computed (see \code{\link[base]{log}}).
# @param ... Currently not used.
# @details TODO
# @return A length-one \code{\link{numeric}} vector.
# @family diversity index
# @author N. Frerebeau
# chaoIndex <- function(n, method = c("MLEU", "CHAO"), k = 10, base = exp(1)) {
#   method <- match.arg(method, several.ok = FALSE)
#
#   f <- function(i, n) { sum(n == i) }
#   N <- sum(n)
#   # p estimation
#   p <- n / N
#   C_e <- 1 - (f(1, n) / N)
#   p_e <- p * C_e
#   # s estimation
#   S.abun <- sum(sapply(X = (k + 1):N, FUN = f, n))
#   S.rare <- sum(sapply(X = 1:k, FUN = f, n))
#   C.rare <- 1 - sum(c(1:k) * sapply(X = 1:k, FUN = f, n))
#   a <- sum(1:k * c(1:k - 1) * sapply(X = 1:k, FUN = f, n))
#   b <- sum(1:k * sapply(X = 1:k, FUN = f, n)) *
#     sum(1:k * sapply(X = 1:k, FUN = f, n) - 1)
#   gamma <- max((S.rare / C.estimate) * (a / b) - 1, 0)
#   S.estimate <- S.abun + (S.rare / C.rare) + (f(1, n) / C.rare) * gamma^2
#
#   H <- switch (
#     methode,
#     MLEU = shannon.index(n, base) + (S.estimate - 1) / (2 * N),
#     CHAO = -sum((p_e * log(p_e, base)) / (1 - (1 - p_e)^N))
#   )
#   return(H)
# }
