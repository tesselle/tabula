#' @include AllGenerics.R AllClasses.R
NULL

similarityIndex <- function(object, method, ...) {
  index <- switch (
    method,
    binomial = binomialSimilarity,
    brainerd = brainerdSimilarity,
    bray = braySimilarity,
    jaccard = jaccardSimilarity,
    morisita = morisitaSimilarity,
    sorenson = sorensonSimilarity,
    stop(paste("there is no such method:", method, sep = " "))
  )

  # Pairwise comparison
  by_row <- method != "binomial"
  m <- if (by_row) nrow(object) else ncol(object)
  labels <- if (by_row) rownames(object) else colnames(object)
  diag_value <- switch (
    method,
    "brainerd" = 200,
    "binomial" = 0,
    1
  )

  if (by_row) {
    # Sample/case comparisons
    beta <- apply(X = utils::combn(1:m, 2), MARGIN = 2, FUN = function(x) {
      index(object[x[1], ], object[x[2], ])
    })
  } else {
    # Taxa/type comparisons
    beta <- apply(X = utils::combn(1:m, 2), MARGIN = 2, FUN = function(x) {
      index(object[, x[1]], object[, x[2]])
    })
  }

  # Matrix of results
  C <- matrix(data = diag_value, nrow = m, ncol = m,
              dimnames = list(labels, labels))
  C[lower.tri(C, diag = FALSE)] <- beta
  C <- t(C)
  C[lower.tri(C, diag = FALSE)] <- beta

  return(C)
}

#' @export
#' @rdname similarity-method
#' @aliases similarity,CountMatrix-method
setMethod(
  f = "similarity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("brainerd", "bray", "jaccard",
                                           "morisita", "sorenson", "binomial"),
                        ...) {
    method <- match.arg(method, several.ok = FALSE)
    C <- similarityIndex(object, method)
    methods::new("SimilarityMatrix", C, method = method)
  }
)

#' @export
#' @rdname similarity-method
#' @aliases similarity,IncidenceMatrix-method
setMethod(
  f = "similarity",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("jaccard", "sorenson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    C <- similarityIndex(object, method)
    methods::new("SimilarityMatrix", C, method = method)
  }
)

# Qualitative index ============================================================
#' Jaccard ----------------------------------------------------------------------
#' Jaccard similarity index
#'
#' @param x A length-p \code{\link{numeric}} vector.
#' @param y A length-p \code{\link{numeric}} vector.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family similarity index
#' @rdname jaccard-index
#' @noRd
jaccardSimilarity <- function(x, y) {
  # Validation
  if (length(x) != length(y))
    stop("a and b should have the same length")
  # presence/absence
  x <- x > 0
  y <- y > 0

  a <- sum(x)
  b <- sum(y)
  j <- sum((x + y) == 2)
  Cj <- j / (a + b - j)
  return(Cj)
}

# Sorenson ---------------------------------------------------------------------
#' Sorenson similarity index
#'
#' @param x A length-p \code{\link{numeric}} vector.
#' @param y A length-p \code{\link{numeric}} vector.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family similarity index
#' @rdname sorenson-index
#' @noRd
sorensonSimilarity <- function(x, y) {
  # Validation
  if (length(x) != length(y))
    stop("a and b should have the same length")
  # presence/absence
  x <- x > 0
  y <- y > 0

  a <- sum(x)
  b <- sum(y)
  j <- sum((x + y) == 2)
  Cs <- 2 * j / (a + b)
  return(Cs)
}

# Quantitative index ===========================================================
# Sorenson ---------------------------------------------------------------------
#' Sorenson quantitative index
#'
#' Bray and Curtis modified version of the Sorenson index.
#' @param x A length-p \code{\link{numeric}} vector.
#' @param y A length-p \code{\link{numeric}} vector.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family similarity index
#' @rdname bray-index
#' @noRd
braySimilarity <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("Numeric values are expected")
  if (length(x) != length(y))
    stop("x and y should have the same length")

  a <- sum(x)
  b <- sum(y)

  j <- sum(apply(X = rbind(x, y), MARGIN = 2, FUN = min))
  Cs <- 2 * j / (a + b)
  return(Cs)
}

# Morisita-Horn ----------------------------------------------------------------
#' Morisita-Horn quantitative index
#'
#' @param x A length-p \code{\link{numeric}} vector.
#' @param y A length-p \code{\link{numeric}} vector.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family similarity index
#' @rdname morisita-index
#' @noRd
morisitaSimilarity <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("numeric values are expected")
  if (length(x) != length(y))
    stop("x and y should have the same length")

  a <- sum(x) # Number of individuals in site A
  b <- sum(y) # Number of individuals in site B
  da <- sum(x^2) / a^2
  db <- sum(y^2) / b^2

  Cm <- (2 * sum(x * y)) / ((da + db) * a * b)
  return(Cm)
}

# Brainerd-Robinson ------------------------------------------------------------
#' Brainerd-Robinson quantitative index
#'
#' @param x A length-p \code{\link{numeric}} vector.
#' @param y A length-p \code{\link{numeric}} vector.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family similarity index
#' @rdname brainerd-index
#' @noRd
brainerdSimilarity <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("numeric values are expected")
  if (length(x) != length(y))
    stop("x and y should have the same length")

  a <- x / sum(x)
  b <- y / sum(y)
  Cb <- 2 - sum(abs(a - b))
  return(Cb * 100)
}

# Binomial co-occurrence -------------------------------------------------------
#' Binomial co-occurrence of types assessment
#'
#' @param x A length-p \code{\link{numeric}} vector.
#' @param y A length-p \code{\link{numeric}} vector.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family similarity index
#' @rdname binomial-index
#' @noRd
binomialSimilarity <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("numeric values are expected")
  if (length(x) != length(y))
    stop("x and y should have the same length")

  # Total number of assemblages
  N <- length(x)
  # Expected proportion of co-occurrences for artifact classes
  p <- sum(x > 0) * sum(y > 0) / N^2
  # Number of observed co-occurence for artifact classes
  o <- sum((x > 0) + (y > 0) == 2)
  if (p == 1) {
    # Avoid NaN generation
    # Print warning ?
    Cbi <- 0
  } else {
    Cbi <- (o - N * p) / sqrt(N * p * (1 - p))
  }
  return(Cbi)
}
