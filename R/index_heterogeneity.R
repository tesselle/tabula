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
  definition = function(object, method = c("berger", "boone", "brillouin",
                                           "mcintosh", "shannon", "simpson"),
                        j = NULL) {
    method <- match.arg(method, several.ok = FALSE)
    by_row <- any(method != "boone")
    index <- index_diversity(object, method, evenness = FALSE, j = j,
                             by_row = by_row)
    .HeterogeneityIndex(index)
  }
)

#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,data.frame-method
setMethod(
  f = "heterogeneity",
  signature = signature(object = "data.frame"),
  definition = function(object, method = c("berger", "boone", "brillouin",
                                           "mcintosh", "shannon", "simpson"),
                        j = NULL) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method, j = j)
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
    ## Validation
    arkhe::assert_count(x)

    x <- x[x > 0] # Remove zeros
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    Nmax <- max(x)
    N <- sum(x)
    d <- Nmax / N
    d
  }
)

## Boone -----------------------------------------------------------------------
#' @export
#' @rdname heterogeneity
#' @aliases index_boone,matrix-method
setMethod(
  f = "index_boone",
  signature = signature(x = "matrix"),
  definition = function(x, j = NULL, na.rm = FALSE, ...) {
    ## Validation
    arkhe::assert_count(x)

    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    Y <- colSums(x) # Site-wide totals of each artifact class
    if (is.null(j)) j <- which.max(Y)

    W <- Y[j] / Y # Weighting factor
    P <- W * Y / sum(W * Y)

    W <- matrix(W, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
    P <- matrix(P, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)

    Wx <- W * x # Weighted counts
    px <- Wx / rowSums(Wx) # Weighted percentages

    rowSums((px - P)^2)
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
    ## Validation
    arkhe::assert_count(x)

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
    ## Validation
    arkhe::assert_count(x)

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
    ## Validation
    arkhe::assert_count(x)

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
    ## Validation
    arkhe::assert_count(x)

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
