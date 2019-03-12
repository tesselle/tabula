# HELPERS TO CHECK DATA

#' Binary numbers
#'
#' Checks if an object only contains 0s and 1s.
#' @param x A \code{\link{numeric}} object to be checked.
#' @return A \code{\link{logical}}.
#' @noRd
isBinary <- function(x) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  sum(x == 1 | x == 0) == length(x)
}

#' Equality within a vector
#'
#' Checks for equality among all elements of a vector.
#' @param x A \code{\link{numeric}} vector to be checked.
#' @param tolerance A length-one \link{\code{numeric}} vector giving the
#'  tolerance to check within.
#' @param na.rm A \code{\link{logical}} scalar specifying if missing values
#'  (including NaN) should be omitted.
#' @return A \code{\link{logical}}.
#' @noRd
isEqual <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
}

#' Trends
#'
#' Checks if a sequence of numbers is monotonically increasing or decreasing.
#' @param x A \code{\link{numeric}} vector to be checked.
#' @param na.rm A \code{\link{logical}} scalar specifying if missing values
#'  (including NaN) should be omitted.
#' @return A \code{\link{logical}}.
#' @rdname trends
#' @noRd
isIncreasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  all(x == cummax(x), na.rm = na.rm) & sum(x, na.rm = na.rm) != 0
}
#' @rdname trends
#' @noRd
isDecreasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  all(x == cummin(x), na.rm = na.rm) & sum(x, na.rm = na.rm) != 0
}

#' Overlap
#'
#' Checks if two data ranges overlap at all.
#' @param x A \code{\link{numeric}} vector.
#' @param y A \code{\link{numeric}} vector.
#' @return A \code{\link{logical}}.
#' @noRd
isOverlapping <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("Numeric vectors are expected.")

  !(min(x) > max(y) | max(x) < min(y))
}

#' Positive numbers
#'
#' Checks if an object only contains positive values.
#' @param x A \code{\link{numeric}} object to be checked.
#' @param strict A \code{\link{logical}} scalar.
#' @param na.rm A \code{\link{logical}} scalar specifying if missing values
#'  (including NaN) should be omitted.
#' @return A \code{\link{logical}}.
#' @noRd
isPositive <- function(x, strict = FALSE, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")

  if (strict) {
    !any(x <= 0, na.rm = na.rm)
  } else {
    !any(x < 0, na.rm = na.rm)
  }
}

#' Square matrix
#'
#' Checks if a matrix is square.
#' @param x A \code{\link{matrix}} to be checked.
#' @return A \code{\link{logical}}.
#' @noRd
isSquare <- function(x) {
  if (is.matrix(x)) {
    nrow(x) == ncol(x)
  } else {
    FALSE
  }
}

#' Contain
#'
#' Checks if a vector is a strict subset of another one.
#' @param subset A vector.
#' @param set A vector.
#' @return A \code{\link{logical}}.
#' @noRd
isSubset <- function(subset, set) {
  # Validation
  n_sub <- length(subset)
  n_set <- length(set)

  if (n_sub > n_set) {
    FALSE
  } else {
    if (n_sub == n_set) {
      identical(subset, set)
    } else {
      # Split 'set' in chunks of adjacent elements
      # Each chunk as the same length as 'subset'
      set2 <- c(set[-n_set], set[-1])
      k <- length(set2) %% n_sub

      m <- matrix(c(set2, set2[1:(n_sub - k)]), ncol = n_sub, byrow = TRUE)
      any(apply(X = m, MARGIN = 1, FUN = identical, y = subset))
    }
  }
}

#' Symmetric matrix
#'
#' Checks if a matrix is symmetric.
#' @param x A \code{\link{matrix}} to be checked.
#' @return A \code{\link{logical}}.
#' @noRd
isSymmetric <- function(x) {
  if (is.matrix(x)) {
    identical(x, t(x))
  } else {
    FALSE
  }
}

#' Integer numbers
#'
#' Checks if an object only contains integer numbers.
#' @param x A \code{\link{numeric}} object to be checked.
#' @param tolerance A length-one \link{\code{numeric}} vector giving the
#'  tolerance to check within.
#' @return A \code{\link{logical}} depending on whether \code{x} contains integer
#'  numbers.
#' @seealso \code{\link[base]{is.integer}}
#' @noRd
isWholeNumber <- function(x, tolerance = .Machine$double.eps^0.5) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  abs(x - round(x)) <= tolerance
}
