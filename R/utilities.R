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

# Matrix constructor
#
# @inheritParams base::matrix
# @param rows A \code{link{logical}} scalar indicating if the number of rows is
#  unspecified.
# @param cols A \code{link{logical}} scalar indicating if the number of columns
#  is unspecified.
# @return A \link{\code{matrix}}.
buildMatrix <- function(data, nrow, ncol, byrow, dimnames,
                        rows = FALSE, cols = FALSE) {
  k <- length(data)
  if (rows) nrow <- k / ncol
  if (cols) ncol <- k / nrow
  if (is.null(dimnames)) {
    dimnames <- list(1:nrow, paste("V", 1:ncol, sep = ""))
  } else {
    if (is.null(dimnames[[1]])) dimnames[[1]] <- 1:nrow
    if (is.null(dimnames[[2]])) dimnames[[2]] <- paste("V", 1:ncol, sep = "")
  }
  M <- matrix(data, nrow, ncol, byrow, dimnames)
  return(M)
}
