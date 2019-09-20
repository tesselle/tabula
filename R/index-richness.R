#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname richness
#' @aliases richness,CountMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("ace", "chao1",
                                           "margalef", "menhinick", "none"),
                        unbiased = FALSE, improved = FALSE, k = 10,
                        simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- lapply(
      X = method,
      FUN = function(x, object, unbiased, improved, k) {
        index <- switch (
          x,
          ace = richnessACE,
          chao1 = richnessChao1,
          margalef = richnessMargalef,
          menhinick = richnessMenhinick,
          none = function(x, ...) { sum(x > 0) },
          stop(sprintf("There is no such method: %s.", method), call. = FALSE)
        )
        apply(X = object, MARGIN = 1, FUN = index, unbiased = unbiased,
              improved = improved, k = k)
      },
      object,
      unbiased,
      improved,
      k
    )
    names(E) <- method
    if (simplify)
      E <- simplify2array(E, higher = FALSE)
    return(E)
  }
)

#' @export
#' @rdname richness
#' @aliases richness,IncidenceMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("chao2", "ice"),
                        unbiased = FALSE, improved = FALSE, k = 10,
                        simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- lapply(
      X = method,
      FUN = function(x, object, unbiased, improved, k) {
        index <- switch (
          x,
          ice = richnessICE,
          chao2 = richnessChao2,
          stop(sprintf("There is no such method: %s.", method), call. = FALSE)
        )
        index(object, unbiased = unbiased, improved = improved, k = k)
      },
      object,
      unbiased,
      improved,
      k
    )
    names(E) <- method
    if (simplify)
      E <- simplify2array(E, higher = FALSE)
    return(E)
  }
)

# ==============================================================================
#' Richness index
#'
#' Abundance data:
#' \code{richnessACE} returns Abundance-based Coverage Estimator (ACE).
#' \code{richnessChao1} returns (improved) Chao1 estimator for abundance data.
#' \code{richnessMargalef} returns Margalef richness index.
#' \code{richnessMenhinick} returns Menhinick richness index.
#'
#' Incidence data:
#' \code{richnessICE} returns Incidence-based Coverage Estimator (ICE).
#' \code{richnessChao2} returns (improved) Chao2 estimator for replicated
#' incidence data.
#' @param x A \code{\link{numeric}} vector giving the number of individuals for
#'  each type (abundance data) or a \code{\link{logical}} matrix (replicated
#'  incidence data).
#' @param k A \code{\link{numeric}} vector giving the threshold between rare and
#'  abundant species.
#' @param unbiased A \code{\link{logical}} scalar. Should the bias-corrected
#'  estimator be used?
#' @param improved A \code{\link{logical}} scalar. Should the improved
#'  estimator be used?
#' @param ... Currently not used.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @references
#'  Chao, A. (1984). Nonparametric Estimation of the Number of Classes in a
#'  Population. \emph{Scandinavian Journal of Statistics}, 11(4), 265-270.
#'
#'  Chao, A. (1987). Estimating the Population Size for Capture-Recapture Data
#'  with Unequal Catchability. \emph{Biometrics} 43(4): 783.
#'  DOI: \href{https://doi.org/10.2307/2531532}{10.2307/2531532}.
#'
#'  Chao, A., & Lee, S.-M. (1992). Estimating the Number of Classes via Sample
#'  Coverage. \emph{Journal of the American Statistical Association}, 87(417),
#'  210-217.
#'  DOI: \href{https://doi.org/10.1080/01621459.1992.10475194}{10.1080/01621459.1992.10475194}
#'
#'  Chiu, C.-H., Wang, Y.-T., Walther, B. A. & Chao, A. (2014). An improved
#'  nonparametric lower bound of species richness via a modified good-turing
#'  frequency formula. \emph{Biometrics}, 70(3), 671-682.
#'  DOI: \href{https://doi.org/10.1111/biom.12200}{10.1111/biom.12200}.
#'
#'  Margalef, R. (1958). Information Theory in Ecology. \emph{General Systems},
#'  3, 36-71.
#'
#'  Menhinick, E. F. (1964). A Comparison of Some Species-Individuals Diversity
#'  Indices Applied to Samples of Field Insects. \emph{Ecology}, 45(4), 859-861.
#'  DOI: \href{https://doi.org/10.2307/1934933}{10.2307/1934933}.
#' @family diversity measures
#' @name index-richness
#' @keywords internal
#' @noRd

# Abundance data ---------------------------------------------------------------
# @rdname index-richness
richnessACE <- function(x, k = 10, ...) {
  # Validation
  check_type(x, expected = "numeric")
  check_scalar(k, expected = "numeric")

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

# @rdname index-richness
richnessChao1 <- function(x, unbiased = FALSE, improved = FALSE, ...) {
  # Validation
  check_type(x, expected = "numeric")
  check_scalar(unbiased, expected = "logical")
  check_scalar(improved, expected = "logical")

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

# @rdname index-richness
richnessMargalef <- function(x, ...) {
  # Validation
  check_type(x, expected = "numeric")

  x <- x[x > 0] # Remove unobserved species
  N <- sum(x) # Number of individuals
  S <- length(x) # Number of observed species
  D <- (S - 1) / log(N, base = exp(1))
  D
}

# @rdname index-richness
richnessMenhinick <- function(x, ...) {
  # Validation
  check_type(x, expected = "numeric")

  x <- x[x > 0] # Remove unobserved species
  N <- sum(x) # Number of individuals
  S <- length(x) # Number of observed species
  D <- S / sqrt(N)
  D
}

# Incidence data ---------------------------------------------------------------
# @rdname index-richness
richnessICE <- function(x, k = 10, ...) {
  # Validation
  check_type(x, expected = "logical")
  check_scalar(k, expected = "numeric")

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

# @rdname index-richness
richnessChao2 <- function(x, unbiased = FALSE, improved = FALSE, ...) {
  # Validation
  check_type(x, expected = "logical")
  check_scalar(unbiased, expected = "logical")
  check_scalar(improved, expected = "logical")

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
