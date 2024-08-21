# ALPHA DIVERSITY
#' @include AllGenerics.R
NULL

# Observed =====================================================================
#' @export
#' @rdname observed
#' @aliases observed,numeric-method
setMethod(
  f = "observed",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    sum(x > 0, na.rm = na.rm)
  }
)

nobserved <- function(x, n, na.rm = FALSE) {
  sum(x == n, na.rm = na.rm)
}

#' @export
#' @rdname observed
#' @aliases singleton,numeric-method
setMethod(
  f = "singleton",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    nobserved(x, n = 1, na.rm = na.rm)
  }
)

#' @export
#' @rdname observed
#' @aliases doubleton,numeric-method
setMethod(
  f = "doubleton",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    nobserved(x, n = 2, na.rm = na.rm)
  }
)

# ACE ==========================================================================
#' @export
#' @rdname index_ace
#' @aliases index_ace,numeric-method
setMethod(
  f = "index_ace",
  signature = signature(x = "numeric"),
  definition = function(x, k = 10, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    S_rare <- sum(x <= k) # Number of rare species
    S_abund <- sum(x > k) # Number of abundant species
    N_rare <- sum(x[x <= k]) # Number of individuals in the rare species

    F1 <- sum(x == 1) # Number of singleton species
    if (F1 == N_rare)
      stop("ACE is undefined when all rare species are singletons, ",
           "consider using the bias-corrected Chao1 index instead.",
           call. = FALSE)

    ## Sample coverage estimate for rare species
    ## ie. proportion of all individuals in rare species that are not singletons
    C_rare <- 1 - (F1 / N_rare)

    # Coefficient of variation
    a <- sum(vapply(
      X = seq_len(k),
      FUN = function(i, x) { i * (i - 1) * sum(x == i) },
      FUN.VALUE = double(1), x = x)
    )
    g2 <- max((S_rare / C_rare) * (a / (N_rare * (N_rare - 1))) - 1, 0)

    D <- S_abund + (S_rare / C_rare) + (F1 / C_rare) * g2
    D
  }
)

# Berger-Parker ================================================================
#' @export
#' @rdname index_berger
#' @aliases index_berger,numeric-method
setMethod(
  f = "index_berger",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    Nmax <- max(x)
    N <- sum(x)
    d <- Nmax / N
    d
  }
)

# Boone ========================================================================
#' @export
#' @rdname index_boone
#' @aliases index_boone,matrix-method
setMethod(
  f = "index_boone",
  signature = signature(x = "matrix"),
  definition = function(x, j = NULL, na.rm = FALSE, ...) {
    ## Validation
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

# Brillouin ====================================================================
#' @export
#' @rdname index_brillouin
#' @aliases index_brillouin,numeric-method
setMethod(
  f = "index_brillouin",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
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

# Chao1 ========================================================================
#' @export
#' @rdname index_chao1
#' @aliases index_chao1,numeric-method
setMethod(
  f = "index_chao1",
  signature = signature(x = "numeric"),
  definition = function(x, unbiased = FALSE, improved = FALSE, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    S <- length(x) # Number of observed species
    N <- sum(x) # Number of individuals
    f1 <- sum(x == 1) # Number of singleton species
    f2 <- sum(x == 2) # Number of doubleton species

    if (unbiased) {
      D <- S + (((N - 1) / N) * f1 * (f1 - 1)) / (2 * (f2 + 1))
    } else {
      if (f2 == 0) {
        D <- S + ((N - 1) / N) * f1 * ((f1 - 1) / 2)
      } else {
        D <- S + ((N - 1) / N) * (f1^2 / (2 * f2))
      }
    }
    if (improved) {
      f3 <- sum(x == 3) # Number of tripleton species
      f4 <- sum(x == 4) # Number of quadrupleton species
      if (f4 == 0)
        stop("Improved Chao1 estimator is undefined ",
             "when there is no quadrupleton species.", call. = FALSE)

      k <- f1 - ((N - 3) / (N - 1)) * ((f2 * f3) / (2 * f4))
      D <- D + ((N - 3) / N) * (f3 / (4 * f4)) * max(k, 0)
    }

    D
  }
)

# Chao2 ========================================================================
#' @export
#' @rdname index_chao2
#' @aliases index_chao2,matrix-method
setMethod(
  f = "index_chao2",
  signature = signature(x = "matrix"),
  definition = function(x, unbiased = FALSE, improved = FALSE, ...) {
    x <- x > 0 # Convert to incidence

    q <- colSums(x) # Number of species in the assemblage
    q <- q[q > 0] # Remove unobserved species
    S <- length(q) # Number of observed species
    t <- nrow(x) # Total number of sampling units
    q1 <- sum(q == 1) # Number of unique species in the assemblage
    q2 <- sum(q == 2) # Number of duplicate species in the assemblage

    if (unbiased) {
      D <- S + ((t - 1) / t) * q1 * ((q1 - 1) / (2 * (q2 + 1)))
    } else {
      if (q2 == 0) {
        D <- S + ((t - 1) / t) * q1 * ((q1 - 1) / 2)
      } else {
        D <- S + ((t - 1) / t) * (q1^2 / (2 * q2))
      }
    }
    if (improved) {
      q3 <- sum(q == 3) # Number of triple species
      q4 <- sum(q == 4) # Number of quadruple species
      if (q4 == 0)
        stop("Improved Chao2 estimator is undefined ",
             "when there is no quadruple species.", call. = FALSE)

      k <- q1 - ((t - 3) / (t - 1)) * ((q2 * q3) / (2 * q4))
      D <- D + ((t - 3) / (4 * t)) * (q3 / q4) * max(k, 0)
    }

    D
  }
)

# ICE ==========================================================================
#' @export
#' @rdname index_ice
#' @aliases index_ice,matrix-method
setMethod(
  f = "index_ice",
  signature = signature(x = "matrix"),
  definition = function(x, k = 10, ...) {
    x <- x > 0 # Convert to incidence

    q <- colSums(x) # Number of species in the assemblage
    q <- q[q > 0] # Remove unobserved species

    S_infr <- sum(q <= k) # Number of infrequent species
    S_freq <- sum(q > k) # Number of frequent species
    N_infr <- sum(q[q <= k]) # Number of incidences in the infrequent species
    # Number of sampling units that include at least one infrequent species
    t <- sum(rowSums(x[, q <= k]) != 0)

    q1 <- sum(q == 1) # Number of unique species in the assemblage
    if (q1 == N_infr)
      stop("ICE is undefined when all infrequent species are unique, ",
           "consider using the bias-corrected Chao2 estimator instead.",
           call. = FALSE)

    ## Sample coverage estimate
    ## ie. proportion of all incidences of infrequent species that are not uniques
    C_infr <- 1 - (q1 / N_infr)

    ## Coefficient of variation
    a <- sum(vapply(
      X = seq_len(k),
      FUN = function(x, q) { x * (x - 1) * sum(q == x) },
      FUN.VALUE = double(1), q = q)
    )
    b <- sum(vapply(
      X = seq_len(k),
      FUN = function(x, q) { x * sum(q == x) },
      FUN.VALUE = double(1), q = q)
    )
    c <- sum(vapply(
      X = seq_len(k),
      FUN = function(x, q) { x * sum(q == x) - 1 },
      FUN.VALUE = double(1), q = q)
    )
    g2 <- max((S_infr / C_infr) * (t / (t - 1)) * (a / (b * c)) - 1, 0)

    D <- S_freq + S_infr / C_infr + q1 * g2 / C_infr
    D
  }
)

# Margalef =====================================================================
#' @export
#' @rdname index_margalef
#' @aliases index_margalef,numeric-method
setMethod(
  f = "index_margalef",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x) # Number of individuals
    S <- length(x) # Number of observed species
    D <- (S - 1) / log(N, base = exp(1))
    D
  }
)

# McIntosh =====================================================================
#' @export
#' @rdname index_mcintosh
#' @aliases index_mcintosh,numeric-method
setMethod(
  f = "index_mcintosh",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
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

# Menhinick ====================================================================
#' @export
#' @rdname index_menhinick
#' @aliases index_menhinick,numeric-method
setMethod(
  f = "index_menhinick",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x) # Number of individuals
    S <- length(x) # Number of observed species
    D <- S / sqrt(N)
    D
  }
)

# Shannon ======================================================================
#' @export
#' @rdname index_shannon
#' @aliases index_shannon,numeric-method
setMethod(
  f = "index_shannon",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, unbiased = FALSE, ACE = FALSE,
                        base = exp(1), na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x)
    S <- length(x) # richness = number of different species
    p <- x / N
    Hmax <- log(p, base)
    Hmax[is.infinite(Hmax)] <- 0

    H <- -sum(p * Hmax)
    if (unbiased) {
      if (ACE) S <- index_ace(x, ...)
      H <- H + (S - 1) / (2 * N)
    }
    if (evenness) H <- H / log(S)
    H
  }
)

# Simpson ======================================================================
#' @export
#' @rdname index_simpson
#' @aliases index_simpson,numeric-method
setMethod(
  f = "index_simpson",
  signature = signature(x = "numeric"),
  definition = function(x, evenness = FALSE, unbiased = FALSE, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    N <- sum(x)
    if (unbiased) D <- sum(x * (x - 1)) / (N * (N - 1))
    else D <- sum((x / N)^2)

    if (evenness) {
      D <- 1 / D
      S <- length(x)
      D <- D / S
    }
    D
  }
)

# Squares ======================================================================
#' @export
#' @rdname index_squares
#' @aliases index_squares,numeric-method
setMethod(
  f = "index_squares",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    ## Validation
    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs
    if (anyNA(x)) return(NA)

    S <- length(x) # Number of observed species
    N <- sum(x) # Number of individuals
    f1 <- sum(x == 1) # Number of singleton species

    Ssq <- S + f1^2 * sum(x^2) / (N^2 - f1 * S)
    Ssq
  }
)
