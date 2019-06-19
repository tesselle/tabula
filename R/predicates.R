# PREDICATES

#' Utility Predicates
#'
#' \code{isEmpty} checks if a vector or list is empty.
#'
#' \code{isSubset} checks if a vector is a strict subset of another one.
#' @param x,y,subset,set An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-utils
#' @keywords internal
#' @noRd
isEmpty <- function(x) {
  length(x) == 0
}
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

      m <- matrix(c(set2, set2[seq_len(n_sub - k)]), ncol = n_sub, byrow = TRUE)
      any(apply(X = m, MARGIN = 1, FUN = identical, y = subset))
    }
  }
}

# ==============================================================================
#' Type Predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-type
#' @keywords internal
#' @noRd
isList <- function(x) {
  typeof(x) == "list"
}
isAtomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double",
                   "complex", "character", "raw")
}
isVector <- function(x) {
  isAtomic(x) || isList(x)
}
isNumeric <- function(x) {
  typeof(x) %in% c("integer", "double")
}
isInteger <- function(x) {
  typeof(x) == "integer"
}
isDouble <- function(x) {
  typeof(x) == "double"
}
isCharacter <- function(x) {
  typeof(x) == "character"
}
isLogical <- function(x) {
  typeof(x) == "logical"
}
# isError <- function(x) {
#   inherits(x, "try-error")
# }

# ==============================================================================
#' Scalar Type Predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-scalar
#' @keywords internal
#' @noRd
isScalarList <- function(x) {
  isList(x) && length(x) == 1
}
isScalarAtomic <- function(x) {
  isAtomic(x) && length(x) == 1
}
isScalarVector <- function(x) {
  isVector(x) && length(x) == 1
}
isScalarNumeric <- function(x) {
  isNumeric(x) && length(x) == 1
}
isScalarInteger <- function(x) {
  isInteger(x) && length(x) == 1
}
isScalarDouble <- function(x) {
  isDouble(x) && length(x) == 1
}
isScalarCharacter <- function(x) {
  isCharacter(x) && length(x) == 1
}
isScalarLogical <- function(x) {
  isLogical(x) && length(x) == 1
}

# ==============================================================================
#' Numeric Predicates
#'
#' Check numeric objects:
#'
#' \code{isOdd} checks if a number is odd.
#'
#' \code{isBinary} checks if an object contains only 0s and 1s.
#'
#' \code{isEqual} checks for equality among all elements of a vector.
#'
#' \code{isIncreasing} and \code{isDecreasing} check if a sequence of numbers is
#' monotonically increasing or decreasing.
#'
#' \code{isOverlapping} checks if two data ranges overlap at all.
#'
#' \code{isPositive} checks if an object only contains positive values.
#'
#' \code{isWholeNumber} checks if an object only contains integer (whole)
#' numbers.
#' @param x,y A \code{\link{numeric}} object to be tested.
#' @param tolerance A \code{\link{numeric}} scalar giving the
#'  tolerance to check within.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted?
#' @return A \code{\link{logical}} scalar.
#' @name predicate-numeric
#' @keywords internal
#' @noRd
isOdd <- function(x) {
  as.logical(x %% 2)
}
isPositive <- function(x, strict = FALSE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (strict) x > 0 else x >= 0
}
isWholeNumber <- function(x, tolerance = .Machine$double.eps^0.5) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  abs(x - round(x, digits = 0)) <= tolerance
}
isBinary <- function(x) {
  if (!isNumeric(x))
    stop(sprintf("`x` must be a numeric vector, not %.", typeof(x)))
  x %in% c(0, 1)
}
isEqual <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  # Validation
  if (!isNumeric(x))
    stop(sprintf("`x` must be a numeric vector, not %.", typeof(x)))
  k <- abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
  if (is.na(k)) k <- FALSE
  k
}
isIncreasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  k <- all(x == cummax(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}
isDecreasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  k <- all(x == cummin(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}
isOverlapping <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("`x` and `y` must be numeric vectors.")
  min(x) <= max(y) && max(x) >= min(y)
}

# ==============================================================================
#' Matrix Predicates
#'
#' \code{isSquare} checks if a matrix is square.
#'
#' \code{isSymmetric} checks if a matrix is symmetric.
#' @param x A \code{\link{matrix}} to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-matrix
#' @keywords internal
#' @noRd
isSquare <- function(x) {
  if (is.matrix(x)) nrow(x) == ncol(x) else FALSE
}
isSymmetric <- function(x) {
  if (is.matrix(x)) identical(x, t(x)) else FALSE
}
