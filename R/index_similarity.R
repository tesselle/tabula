# SIMILARITY
#' @include AllGenerics.R AllClasses.R
NULL

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
    index_similarity(object, method)
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
    index_similarity(object, method)
  }
)

index_similarity <- function(object, method, ...) {
  fun <- switch_similarity(method) # Select method

  # Pairwise comparison
  by_row <- method != "binomial"
  if (by_row) { # Sample/case comparisons
    m <- nrow(object)
    labels <- rownames(object)
    beta <- function(x, f) {
      f(object[x[1], ], object[x[2], ])
    }
  } else { # Taxa/type comparisons
    m <- ncol(object)
    labels <- colnames(object)
    beta <- function(x, f) {
      f(object[, x[1]], object[, x[2]])
    }
  }

  diag_value <- switch (
    method,
    "brainerd" = 200,
    "binomial" = 0,
    1
  )

  cbn <- utils::combn(seq_len(m), 2)
  index <- apply(X = cbn, MARGIN = 2, FUN = beta, f = fun)

  # Matrix of results
  mtx <- matrix(data = diag_value, nrow = m, ncol = m,
                dimnames = list(labels, labels))
  mtx[lower.tri(mtx, diag = FALSE)] <- index
  mtx <- t(mtx)
  mtx[lower.tri(mtx, diag = FALSE)] <- index

  sim <- stats::as.dist(mtx)
  attr(sim, "method") <- method
  sim
}

# Index ========================================================================
switch_similarity <- function(x) {
  index <- switch (
    x,
    binomial = similarityBinomial,
    brainerd = similarityBrainerd,
    bray = similarityBray,
    jaccard = similarityJaccard,
    morisita = similarityMorisita,
    sorenson = similaritySorenson,
    stop(sprintf("There is no such method: %s.", x), call. = FALSE)
  )
}

#' Similarity index
#'
#' @description
#' Qualitative index:
#' \code{similarityJaccard} returns Jaccard similarity index.
#' \code{similaritySorenson} returns Sorenson similarity index.
#'
#' Quantitative index:
#' \code{similarityBray} returns Bray and Curtis modified version of the
#' Sorenson index.
#' \code{similarityMorisita} returns Morisita-Horn quantitative index.
#' \code{similarityBrainerd} returns Brainerd-Robinson quantitative index.
#' \code{similarityBinomial} returns binomial co-occurrence of types assessment.
#' @param x A length-p \code{\link{numeric}} vector.
#' @param y A length-p \code{\link{numeric}} vector.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family diversity measures
#' @name index-similarity
#' @keywords internal
#' @noRd

# Qualitative index ------------------------------------------------------------
# @rdname index-similarity
similarityJaccard <- function(x, y) {
  # Validation
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)
  # presence/absence
  x <- x > 0
  y <- y > 0

  a <- sum(x)
  b <- sum(y)
  j <- sum((x + y) == 2)
  Cj <- j / (a + b - j)
  return(Cj)
}

# @rdname index-similarity
similaritySorenson <- function(x, y) {
  # Validation
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)
  # presence/absence
  x <- x > 0
  y <- y > 0

  a <- sum(x)
  b <- sum(y)
  j <- sum((x + y) == 2)
  Cs <- 2 * j / (a + b)
  return(Cs)
}

# Quantitative index -----------------------------------------------------------
# @rdname index-similarity
similarityBray <- function(x, y) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (!is.numeric(y))
    stop("`y` must be a numeric vector.")
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)

  a <- sum(x)
  b <- sum(y)

  j <- sum(apply(X = rbind(x, y), MARGIN = 2, FUN = min))
  Cs <- 2 * j / (a + b)
  return(Cs)
}

# @rdname index-similarity
similarityMorisita <- function(x, y) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (!is.numeric(y))
    stop("`y` must be a numeric vector.")
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)

  a <- sum(x) # Number of individuals in site A
  b <- sum(y) # Number of individuals in site B
  da <- sum(x^2) / a^2
  db <- sum(y^2) / b^2

  Cm <- (2 * sum(x * y)) / ((da + db) * a * b)
  return(Cm)
}

# @rdname index-similarity
similarityBrainerd <- function(x, y) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (!is.numeric(y))
    stop("`y` must be a numeric vector.")
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)

  a <- x / sum(x)
  b <- y / sum(y)
  Cb <- 2 - sum(abs(a - b))
  return(Cb * 100)
}

# Binomial co-occurrence -------------------------------------------------------
# @rdname index-similarity
similarityBinomial <- function(x, y) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (!is.numeric(y))
    stop("`y` must be a numeric vector.")
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)

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
