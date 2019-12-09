#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname heterogeneity-index
#' @aliases index_heterogeneity,CountMatrix-method
setMethod(
  f = "index_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        jackknife = TRUE, bootstrap = TRUE, simulate = FALSE,
                        level = 0.80, n = 1000, ...) {
    fun <- switch_heterogeneity(method)
    index <- index_diversity(object, fun, jackknife, bootstrap, simulate,
                             prob = NULL, level = level, n = n, ...)
    index@method <- method[[1L]]
    methods::as(index, "HeterogeneityIndex")
  }
)

#' @export
#' @rdname heterogeneity-index
#' @aliases index_evenness,CountMatrix-method
setMethod(
  f = "index_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson"),
                        jackknife = TRUE, bootstrap = TRUE, simulate = FALSE,
                        level = 0.80, n = 1000, ...) {
    fun <- switch_evenness(method)
    index <- index_diversity(object, fun, jackknife, bootstrap, simulate,
                             prob = NULL, level = level, n = n, ...)
    index@method <- method[[1L]]
    methods::as(index, "EvennessIndex")
  }
)

switch_heterogeneity <- function(x) {
  # Validation
  measures <- c("berger", "brillouin", "mcintosh", "shannon", "simpson")
  x <- match.arg(x, choices = measures, several.ok = FALSE)

  index <- switch (
    x,
    berger = dominanceBerger,
    brillouin = diversityBrillouin,
    mcintosh = dominanceMcintosh,
    shannon = diversityShannon,
    simpson = dominanceSimpson,
    stop(sprintf("There is no such method: %s.", x), call. = FALSE)
  )
  return(index)
}

switch_evenness <- function(x) {
  # Validation
  measures <- c("shannon", "brillouin", "mcintosh", "simpson")
  x <- match.arg(x, choices = measures, several.ok = FALSE)

  index <- switch (
    x,
    brillouin = evennessBrillouin,
    mcintosh = evennessMcintosh,
    shannon = evennessShannon,
    simpson = evennessSimpson,
    stop(sprintf("There is no such method: %s.", x), call. = FALSE)
  )
  return(index)
}

# ==============================================================================
#' Diversity, dominance and evenness index
#'
#' \code{dominanceBerger} returns Berger-Parker dominance index.
#' \code{diversityBrillouin} and \code{evennessBrillouin} return Brillouin
#' diversity index and evenness.
#' \code{dominanceMcintosh} and \code{evennessMcintosh} return McIntosh
#' dominance index and evenness.
#' \code{diversityShannon}, \code{evennessShannon}, \code{varianceShannon}
#' return Shannon-Wiener diversity index, evenness and variance.
#' \code{dominanceSimpson} and \code{evennessSimpson} return Simpson dominance
#' index and evenness.
#' @param x A \code{\link{numeric}} vector giving the number of individuals for
#'  each class.
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @references
#'  Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera
#'  in Deep-Sea Sediments. \emph{Science}, 168(3937), 1345-1347.
#'  DOI: \href{https://doi.org/10.1126/science.168.3937.1345}{10.1126/science.168.3937.1345}.
#'
#'  Brillouin, L. (1956). \emph{Science and information theory}. New York:
#'  Academic Press.
#'
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. \emph{Ecology}, 48(3), 392-404.
#'  DOI: \href{https://doi.org/10.2307/1932674}{10.2307/1932674}.
#'
#'  Shannon, C. E. (1948). A Mathematical Theory of Communication. \emph{The
#'  Bell System Technical Journal}, 27, 379-423.
#'  DOI: \href{https://doi.org/10.1002/j.1538-7305.1948.tb01338.x}{10.1002/j.1538-7305.1948.tb01338.x}.
#'
#'  Simpson, E. H. (1949). Measurement of Diversity. \emph{Nature}, 163(4148),
#'  688-688. DOI: \href{https://doi.org/10.1038/163688a0}{10.1038/163688a0}.
#' @author N. Frerebeau
#' @family diversity index
#' @name index-heterogeneity
#' @keywords internal
#' @noRd

# Berger-Parker ----------------------------------------------------------------
# @rdname index-heterogeneity
dominanceBerger <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  Nmax <- max(x)
  N <- sum(x)
  d <- Nmax / N
  d
}

# Brillouin --------------------------------------------------------------------
# @rdname index-heterogeneity
diversityBrillouin <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  HB <- (lfactorial(N) - sum(lfactorial(x))) / N
  HB
}
# @rdname index-heterogeneity
evennessBrillouin <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  HB <- diversityBrillouin(x)
  N <- sum(x)
  S <- length(x) # richness = number of different species
  a <- trunc(N / S)
  r <- N - S * a
  c <- (S - r) * lfactorial(a) + r * lfactorial(a + 1)
  HBmax <- (1 / N) * (lfactorial(N) - c)
  E <- HB / HBmax
  E
}

# McIntosh ---------------------------------------------------------------------
# @rdname index-heterogeneity
dominanceMcintosh <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  U <- sqrt(sum(x^2))
  D <- (N - U) / (N - sqrt(N))
  D
}
# @rdname index-heterogeneity
evennessMcintosh <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  S <- length(x) # richness = number of different species
  U <- sqrt(sum(x^2))
  E <- (N - U) / (N - (N / sqrt(S)))
  E
}

# Shannon ----------------------------------------------------------------------
# @rdname index-heterogeneity
diversityShannon <- function(x, base = exp(1), zero.rm = TRUE, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  if (zero.rm) x <- x[x > 0]

  N <- sum(x)
  S <- length(x) # richness = number of different species
  p <- x / N
  Hmax <- log(p, base)
  Hmax[is.infinite(Hmax)] <- 0

  H <- -sum(p * Hmax)
  H
}
# @rdname index-heterogeneity
evennessShannon <- function(x, zero.rm = TRUE, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  if (zero.rm) x <- x[x > 0]

  S <- length(x)
  E <- diversityShannon(x, zero.rm = zero.rm) / log(S)
  E
}
# @rdname index-heterogeneity
varianceShannon <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  S <- length(x) # richness = number of different species
  p <- x / N
  a <- sum(p * (log(p, base = exp(1)))^2)
  b <- sum(p * log(p, base = exp(1)))^2
  var <- ((a - b) / N) + ((S - 1) / (2 * N^2))
  var
}

# Simpson ----------------------------------------------------------------------
# @rdname index-heterogeneity
dominanceSimpson <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  N <- sum(x)
  D <- sum(x * (x - 1)) / (N* (N - 1)) # For discrete data
  D
}
# @rdname index-heterogeneity
evennessSimpson <- function(x, ...) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  # Remove zeros
  x <- x[x > 0]

  D <- 1 / dominanceSimpson(x)
  S <- length(x[x > 0]) # richness = number of different species
  E <- D / S
  E
}

# Chao -------------------------------------------------------------------------
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
