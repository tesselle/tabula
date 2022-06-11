#' @include AllGenerics.R AllClasses.R
NULL

# Richness =====================================================================
#' @export
#' @rdname richness
#' @aliases richness,matrix-method
setMethod(
  f = "richness",
  signature = signature(object = "matrix"),
  definition = function(object, method = c("count", "margalef", "menhinick")) {
    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method)
    .RichnessIndex(index)
  }
)

#' @export
#' @rdname richness
#' @aliases richness,data.frame-method
setMethod(
  f = "richness",
  signature = signature(object = "data.frame"),
  definition = function(object, method = c("count", "margalef", "menhinick")) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method)
  }
)

# Composition ==================================================================
#' @export
#' @rdname richness
#' @aliases composition,matrix-method
setMethod(
  f = "composition",
  signature = signature(object = "matrix"),
  definition = function(object, method = c("chao1", "ace", "chao2", "ice"),
                        unbiased = FALSE, improved = FALSE, k = 10) {
    method <- match.arg(method, several.ok = FALSE)
    by_row <- any(method == c("chao1", "ace"))
    index <- index_diversity(object, method, unbiased = unbiased,
                             improved = improved, k = k, by_row = by_row)
    .CompositionIndex(index)
  }
)

#' @export
#' @rdname richness
#' @aliases composition,data.frame-method
setMethod(
  f = "composition",
  signature = signature(object = "data.frame"),
  definition = function(object, method = c("chao1", "ace", "chao2", "ice"),
                        unbiased = FALSE, improved = FALSE, k = 10) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method, unbiased = unbiased,
                         improved = improved, k = k)
  }
)

# Index ========================================================================
index_count <- function(x, ...) { sum(x > 0) }

## Abundance data --------------------------------------------------------------
#' @export
#' @rdname richness
#' @aliases index_margalef,numeric-method
setMethod(
  f = "index_margalef",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    ## Validation
    arkhe::assert_count(x)

    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs

    N <- sum(x) # Number of individuals
    S <- length(x) # Number of observed species
    D <- (S - 1) / log(N, base = exp(1))
    D
  }
)

#' @export
#' @rdname richness
#' @aliases index_menhinick,numeric-method
setMethod(
  f = "index_menhinick",
  signature = signature(x = "numeric"),
  definition = function(x, na.rm = FALSE, ...) {
    ## Validation
    arkhe::assert_count(x)

    x <- x[x > 0] # Remove unobserved species
    if (na.rm) x <- stats::na.omit(x) # Remove NAs

    N <- sum(x) # Number of individuals
    S <- length(x) # Number of observed species
    D <- S / sqrt(N)
    D
  }
)

#' @export
#' @rdname richness
#' @aliases index_ace,numeric-method
setMethod(
  f = "index_ace",
  signature = signature(x = "numeric"),
  definition = function(x, k = 10, ...) {
    ## Validation
    arkhe::assert_count(x)

    x <- x[x > 0] # Remove unobserved species
    S <- length(x) # Number of observed species
    S_rare <- sum(x <= k) # Number of rare species
    S_abun <- sum(x > k) # Number of abundant species

    N_rare <- sum(x[x <= k]) # Number of individuals in the rare species
    f1 <- sum(x == 1) # Number of singleton species
    if (f1 == N_rare)
      stop("ACE is undefined when all rare species are singletons, ",
           "consider using the bias-corrected Chao1 index instead.",
           call. = FALSE)
    C_rare <- 1 - (f1 / N_rare) # Sample coverage estimate for rare species
    # ie. proportion of all individuals in rare species that are not singletons

    # Coefficient of variation
    a <- sum(vapply(
      X = seq_len(k),
      FUN = function(i, x) { i * (i - 1) * sum(x == i) },
      FUN.VALUE = double(1), x = x)
    )
    b <- sum(vapply(
      X = seq_len(k),
      FUN = function(i, x) { i * sum(x == i) },
      FUN.VALUE = double(1), x = x)
    )
    c <- sum(vapply(
      X = seq_len(k),
      FUN = function(i, x) { i * sum(x == i) - 1 },
      FUN.VALUE = double(1), x = x)
    )
    g2 <- max((S_rare / C_rare) * (a / (b * c)) - 1, 0)

    D <- S_abun + S_rare / C_rare + f1 * g2 / C_rare
    D
  }
)

#' @export
#' @rdname richness
#' @aliases index_chao1,numeric-method
setMethod(
  f = "index_chao1",
  signature = signature(x = "numeric"),
  definition = function(x, unbiased = FALSE, improved = FALSE, ...) {
    ## Validation
    arkhe::assert_count(x)

    x <- x[x > 0] # Remove unobserved species
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

## Incidence data --------------------------------------------------------------
#' @export
#' @rdname richness
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
    C_infr <- 1 - (q1 / N_infr) # Sample coverage estimate
    # ie. proportion of all incidences of infrequent species that are not uniques

    # Coefficient of variation
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

#' @export
#' @rdname richness
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
