# Integer numbers
#
# Tests if an object only contains integer numbers.
# @param x A \code{\link{numeric}} object to be tested.
# @param tol A length-one \link{\code{numeric}} vector giving the tolerance to
#  check within.
# @return A \link{\code{logical}} depending on whether \code{x} contains integer
#  numbers.
# @seealso \link{\code[base]{is.integer}}
isWholeNumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# Binary numbers
#
# Tests if an object only contains 0s and 1s.
# @param x A \code{\link{numeric}} object to be tested.
# @return A \link{\code{logical}}.
isBinary <- function(x) {
  sum(x == 1 | x == 0) == length(x)
}

# Equality within a vector
#
# Tests for equality among all elements of a vector.
# @param x A \code{\link{numeric}} object to be tested.
# @param tol A length-one \link{\code{numeric}} vector giving the tolerance to
#  check within.
# @param na.rm A \code{\link{logical}} scalar specifying if missing values
#  (including NaN) should be omitted.
# @return A \link{\code{logical}}.
isEqual <- function(x, tol = .Machine$double.eps^0.5, na.rm = TRUE) {
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) < tol
}
