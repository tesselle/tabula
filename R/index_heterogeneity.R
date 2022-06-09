# HETEROGENEITY INDEX
#' @include AllGenerics.R AllClasses.R
NULL

# Heterogeneity ================================================================
#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,matrix-method
setMethod(
  f = "heterogeneity",
  signature = signature(object = "matrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson")) {
    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method, evenness = FALSE)
    .HeterogeneityIndex(index)
  }
)

#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,data.frame-method
setMethod(
  f = "heterogeneity",
  signature = signature(object = "data.frame"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson")) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method)
  }
)

# Evenness =====================================================================
#' @export
#' @rdname heterogeneity
#' @aliases evenness,matrix-method
setMethod(
  f = "evenness",
  signature = signature(object = "matrix"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson")) {
    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method, evenness = TRUE)
    .EvennessIndex(index)
  }
)

#' @export
#' @rdname heterogeneity
#' @aliases evenness,data.frame-method
setMethod(
  f = "evenness",
  signature = signature(object = "data.frame"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson")) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method)
  }
)

# Index ========================================================================
## Berger-Parker ---------------------------------------------------------------
#' @export
#' @rdname heterogeneity
#' @aliases index_berger,numeric-method
setMethod(
  f = "index_berger",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    x <- x[x > 0] # Remove zeros
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    Nmax <- max(x)
    N <- sum(x)
    d <- Nmax / N
    d
  }
)

## Brillouin -------------------------------------------------------------------
#' @export
#' @rdname heterogeneity
#' @aliases index_brillouin,numeric-method
setMethod(
  f = "index_brillouin",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, na.rm = FALSE, ...) {
    x <- x[x > 0] # Remove zeros
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x)
    bri <- (lfactorial(N) - sum(lfactorial(x))) / N

    if (evenness) {
      N <- sum(x)
      S <- length(x) # richness = number of different species
      a <- trunc(N / S)
      r <- N - S * a
      c <- (S - r) * lfactorial(a) + r * lfactorial(a + 1)
      HBmax <- (1 / N) * (lfactorial(N) - c)
      bri <- bri / HBmax
    }

    bri
  }
)

## McIntosh --------------------------------------------------------------------
#' @export
#' @rdname heterogeneity
#' @aliases index_mcintosh,numeric-method
setMethod(
  f = "index_mcintosh",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, na.rm = FALSE, ...) {
    x <- x[x > 0] # Remove zeros
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x)
    S <- length(x) # richness = number of different species
    U <- sqrt(sum(x^2))

    if (evenness) {
      mac <- (N - U) / (N - (N / sqrt(S)))
    } else {
      mac <- (N - U) / (N - sqrt(N))
    }
    mac
  }
)

## Shannon ---------------------------------------------------------------------
#' @export
#' @rdname heterogeneity
#' @aliases index_shannon,numeric-method
setMethod(
  f = "index_shannon",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, base = exp(1),
                        na.rm = FALSE, ...) {
    x <- x[x > 0] # Remove zeros
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x)
    S <- length(x) # richness = number of different species
    p <- x / N
    Hmax <- log(p, base)
    Hmax[is.infinite(Hmax)] <- 0

    sha <- -sum(p * Hmax)

    if (evenness) {
      S <- length(x)
      sha <- sha / log(S)
    }
    sha
  }
)
# @rdname index_shannon
variance_shannon <- function(x, na.rm = FALSE, ...) {
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
#' @export
#' @rdname heterogeneity
#' @aliases index_simpson,numeric-method
setMethod(
  f = "index_simpson",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, na.rm = FALSE, ...) {
    x <- x[x > 0] # Remove zeros
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x)
    D <- sum(x * (x - 1)) / (N* (N - 1)) # For discrete data

    if (evenness) {
      D <- 1 / D
      S <- length(x) # richness = number of different species
      D <- D / S
    }
    D
  }
)

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
