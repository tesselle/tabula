# HETEROGENEITY INDEX
#' @include AllGenerics.R AllClasses.R
NULL

# Heterogeneity ================================================================
#' @export
#' @rdname heterogeneity-index
#' @aliases index_heterogeneity,CountMatrix-method
setMethod(
  f = "index_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_heterogeneity(method) # Select method
    index <- index_diversity(object, fun)
    .HeterogeneityIndex(index, method = method)
  }
)

#' @export
#' @rdname heterogeneity-index
#' @aliases simulate_heterogeneity,CountMatrix-method
setMethod(
  f = "simulate_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        quantiles = TRUE, level = 0.80, step = 1, n = 1000,
                        progress = getOption("tabula.progress"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_heterogeneity(method) # Select method
    index <- simulate_diversity(
      object,
      method = fun,
      quantiles = quantiles,
      level = level,
      step = step,
      n = n,
      progress = progress
    )
    .HeterogeneityIndex(index, method = method)
  }
)

#' @export
#' @rdname heterogeneity-index
#' @aliases bootstrap_heterogeneity,CountMatrix-method
setMethod(
  f = "bootstrap_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        probs = c(0.05, 0.95), n = 1000, ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_heterogeneity(method) # Select method
    bootstrap_diversity(object, method = fun, probs = probs, n = n)
  }
)

#' @export
#' @rdname heterogeneity-index
#' @aliases jackknife_heterogeneity,CountMatrix-method
setMethod(
  f = "jackknife_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_heterogeneity(method) # Select method
    jackknife_diversity(object, method = fun)
  }
)

# Evenness =====================================================================
#' @export
#' @rdname heterogeneity-index
#' @aliases index_evenness,CountMatrix-method
setMethod(
  f = "index_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_evenness(method) # Select method
    index <- index_diversity(object, fun)
    .EvennessIndex(index, method = method)
  }
)

#' @export
#' @rdname heterogeneity-index
#' @aliases simulate_evenness,CountMatrix-method
setMethod(
  f = "simulate_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson"),
                        quantiles = TRUE, level = 0.80, step = 1, n = 1000,
                        progress = getOption("tabula.progress"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_evenness(method) # Select method
    index <- simulate_diversity(
      object,
      method = fun,
      quantiles = quantiles,
      level = level,
      step = step,
      n = n,
      progress = progress
    )
    .EvennessIndex(index, method = method)
  }
)

#' @export
#' @rdname heterogeneity-index
#' @aliases bootstrap_evenness,CountMatrix-method
setMethod(
  f = "bootstrap_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        probs = c(0.05, 0.95), n = 1000, ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_evenness(method) # Select method
    bootstrap_diversity(object, method = fun, probs = probs, n = n)
  }
)

#' @export
#' @rdname heterogeneity-index
#' @aliases jackknife_evenness,CountMatrix-method
setMethod(
  f = "jackknife_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- switch_evenness(method) # Select method
    jackknife_diversity(object, method = fun)
  }
)

# Index ========================================================================
switch_heterogeneity <- function(x) {
  switch (
    x,
    berger = dominanceBerger,
    brillouin = diversityBrillouin,
    mcintosh = dominanceMcintosh,
    shannon = diversityShannon,
    simpson = dominanceSimpson,
    stop(sprintf("There is no such method: %s.", x), call. = FALSE)
  )
}
switch_evenness <- function(x) {
  switch (
    x,
    brillouin = evennessBrillouin,
    mcintosh = evennessMcintosh,
    shannon = evennessShannon,
    simpson = evennessSimpson,
    stop(sprintf("There is no such method: %s.", x), call. = FALSE)
  )
}

#' Diversity, dominance and evenness index
#'
#' @param x A [`numeric`] vector giving the number of individuals for each
#'  class.
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return A length-one [`numeric`] vector.
#' @author N. Frerebeau
#' @family diversity index
#' @name index-heterogeneity
#' @keywords internal
#' @noRd

## Berger-Parker ---------------------------------------------------------------
# @rdname index-heterogeneity
dominanceBerger <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  Nmax <- max(x)
  N <- sum(x)
  d <- Nmax / N
  d
}

## Brillouin -------------------------------------------------------------------
# @rdname index-heterogeneity
diversityBrillouin <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  HB <- (lfactorial(N) - sum(lfactorial(x))) / N
  HB
}
# @rdname index-heterogeneity
evennessBrillouin <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

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

## McIntosh --------------------------------------------------------------------
# @rdname index-heterogeneity
dominanceMcintosh <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  U <- sqrt(sum(x^2))
  D <- (N - U) / (N - sqrt(N))
  D
}
# @rdname index-heterogeneity
evennessMcintosh <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  S <- length(x) # richness = number of different species
  U <- sqrt(sum(x^2))
  E <- (N - U) / (N - (N / sqrt(S)))
  E
}

## Shannon ---------------------------------------------------------------------
# @rdname index-heterogeneity
diversityShannon <- function(x, base = exp(1),
                             zero.rm = TRUE, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  if (zero.rm) x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  S <- length(x) # richness = number of different species
  p <- x / N
  Hmax <- log(p, base)
  Hmax[is.infinite(Hmax)] <- 0

  H <- -sum(p * Hmax)
  H
}
# @rdname index-heterogeneity
evennessShannon <- function(x, zero.rm = TRUE, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  if (zero.rm) x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  S <- length(x)
  E <- diversityShannon(x, zero.rm = zero.rm) / log(S)
  E
}
# @rdname index-heterogeneity
varianceShannon <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  S <- length(x) # richness = number of different species
  p <- x / N
  a <- sum(p * (log(p, base = exp(1)))^2)
  b <- sum(p * log(p, base = exp(1)))^2
  var <- ((a - b) / N) + ((S - 1) / (2 * N^2))
  var
}

## Simpson ---------------------------------------------------------------------
# @rdname index-heterogeneity
dominanceSimpson <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  D <- sum(x * (x - 1)) / (N* (N - 1)) # For discrete data
  D
}
# @rdname index-heterogeneity
evennessSimpson <- function(x, na.rm = FALSE, ...) {
  # Validation
  stopifnot(is.numeric(x))
  x <- x[x > 0] # Remove zeros
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  D <- 1 / dominanceSimpson(x)
  S <- length(x[x > 0]) # richness = number of different species
  E <- D / S
  E
}

## Chao ------------------------------------------------------------------------
# Chao index
#
# @param n A [`numeric`] vector.
# @param method A [`character`] string specifiying the method to be
#  used. This must be one of "`MLEU`", "`CHAO`" (see details). Any unambiguous
#  substring can be given.
# @param k A length-one [`numeric`] vector.
# @param base A positive or complex number: the base with respect to which
#  logarithms are computed (see [log()]).
# @param ... Currently not used.
# @details TODO
# @return A length-one [`numeric`] vector.
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
