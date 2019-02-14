# HELPERS TO CHECK DATA

# Binary numbers
#
# Checks if an object only contains 0s and 1s.
# @param x A \code{\link{numeric}} object to be checked.
# @return A \link{\code{logical}}.
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
# @return A \link{\code{logical}}.
isEqual <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) < tolerance
}

# Trends
#
# Checks if a sequence of numbers is monotonically increasing or decreasing.
# @param x A \code{\link{numeric}} vector to be checked.
# @return A \link{\code{logical}}.
# @rdname trends
isIncreasing <- function(x, na.rm = TRUE) {
  all(x == cummax(x), na.rm = na.rm)
}
isDecreasing <- function(x, na.rm = TRUE) {
  all(x == cummin(x), na.rm = na.rm)
}
isPeak <- function(x, na.rm = TRUE) {
  n <- length(x)
  i <- which.max(x)
  isIncreasing(x[1:i], na.rm = na.rm) &
    isDecreasing(x[i:n], na.rm = na.rm)
}

# Positive numbers
#
# Checks if an object only contains positive values.
# @param x A \code{\link{numeric}} object to be checked.
# @param strict A \code{\link{logical}} scalar.
# @return A \link{\code{logical}}.
isPositive <- function(x, strict = FALSE) {
  if (!any(is.nan(x))) {
    if (strict) {
      !any(x <= 0, na.rm = TRUE)
    } else {
      !any(x < 0, na.rm = TRUE)
    }
  } else {
    FALSE
  }
}

# Square matrix
#
# Checks if a matrix is square.
# @param x A \code{\link{matrix}} to be checked.
# @return A \link{\code{logical}}.
isSquare <- function(x) {
  if (is.matrix(x)) {
    nrow(x) == ncol(x)
  } else {
    FALSE
  }
}

# Symmetric matrix
#
# Checks if a matrix is symmetric.
# @param x A \code{\link{matrix}} to be checked.
# @return A \link{\code{logical}}.
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
# @return A \link{\code{logical}} depending on whether \code{x} contains integer
#  numbers.
# @seealso \link{\code[base]{is.integer}}
isWholeNumber <- function(x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}
