# HELPERS TO CHECK DATA

# Binary numbers
#
# Checks if an object only contains 0s and 1s.
# @param x A \code{\link{numeric}} object to be checked.
# @return A \code{\link{logical}}.
isBinary <- function(x) {
  sum(x == 1 | x == 0) == length(x)
}

# Equality within a vector
#
# Checks for equality among all elements of a vector.
# @param x A \code{\link{numeric}} vector to be checked.
# @param tolerance A length-one \link{\code{numeric}} vector giving the
#  tolerance to check within.
# @param na.rm A \code{\link{logical}} scalar specifying if missing values
#  (including NaN) should be omitted.
# @return A \code{\link{logical}}.
isEqual <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) < tolerance
}

# Trends
#
# Checks if a sequence of numbers is monotonically increasing or decreasing.
# @param x A \code{\link{numeric}} vector to be checked.
# @param na.rm A \code{\link{logical}} scalar specifying if missing values
#  (including NaN) should be omitted.
# @return A \code{\link{logical}}.
# @rdname trends
isIncreasing <- function(x, na.rm = TRUE) {
  all(x == cummax(x), na.rm = na.rm) & sum(x) != 0
}
isDecreasing <- function(x, na.rm = TRUE) {
  all(x == cummin(x), na.rm = na.rm) & sum(x) != 0
}
isPeak <- function(x, na.rm = TRUE) {
  n <- length(x)
  i <- which.max(x)
  isIncreasing(x[1:i], na.rm = na.rm) &
    isDecreasing(x[i:n], na.rm = na.rm)
}

# Overlap
#
# Checks if two data ranges overlap at all.
# @param x A vector of two \code{\link{numeric}} values (range 1).
# @param y A vector of two \code{\link{numeric}} values (range 2).
# @return A \code{\link{logical}}.
isOverlapping <- function(x, y) {
  # Validation
  if (length(x) != 2 | length(y) != 2)
    stop("Vectors of length two are expected (min-max ranges).")

  x <- range(x)
  y <- range(y)
  !(x[1] >= y[2] | x[2] <= y[1]) | sum(x, y) == 0
}

# Positive numbers
#
# Checks if an object only contains positive values.
# @param x A \code{\link{numeric}} object to be checked.
# @param strict A \code{\link{logical}} scalar.
# @return A \code{\link{logical}}.
isPositive <- function(x, strict = FALSE, na.rm = TRUE) {
  x <- as.numeric(x)
  if (!any(is.nan(x))) {
    if (strict) {
      !any(x <= 0, na.rm = na.rm)
    } else {
      !any(x < 0, na.rm = na.rm)
    }
  } else {
    FALSE
  }
}

# Square matrix
#
# Checks if a matrix is square.
# @param x A \code{\link{matrix}} to be checked.
# @return A \code{\link{logical}}.
isSquare <- function(x) {
  if (is.matrix(x)) {
    nrow(x) == ncol(x)
  } else {
    FALSE
  }
}

# Contain
#
# Checks if a vector is a strict subset of another one.
# @param subset A vector.
# @param set A vector.
# @return A \code{\link{logical}}.
isSubset <- function(subset, set) {
  # Validation
  n_sub <- length(subset)
  n_set <- length(set)

  if (n_sub > n_set) {
    FALSE
  } else {
    if (n_sub == n_set) {
      identical(subset, sub)
    } else {
      # Split 'set' in chunks of adjacent elements
      # Each chunk as the same length as 'subset'
      k <- matrix(c(set[-n_set], set[-1]), ncol = n_sub, byrow = TRUE)
      any(apply(X = k, MARGIN = 1, FUN = identical, y = subset))
    }
  }
}

# Symmetric matrix
#
# Checks if a matrix is symmetric.
# @param x A \code{\link{matrix}} to be checked.
# @return A \code{\link{logical}}.
isSymmetric <- function(x) {
  if (is.matrix(x)) {
    identical(x, t(x))
  } else {
    FALSE
  }
}

# Integer numbers
#
# Checks if an object only contains integer numbers.
# @param x A \code{\link{numeric}} object to be checked.
# @param tolerance A length-one \link{\code{numeric}} vector giving the
#  tolerance to check within.
# @return A \code{\link{logical}} depending on whether \code{x} contains integer
#  numbers.
# @seealso \code{\link[base]{is.integer}}
isWholeNumber <- function(x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}
