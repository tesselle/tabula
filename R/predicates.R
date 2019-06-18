# PREDICATES

#' Utility predicates
#'
#' \code{isEmpty} checks if a vector or list is empty.
#' \code{isSubset} checks if a vector is a strict subset of another one.
#' @param x,y,subset,set An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-utils
#' @keywords internal
NULL

#' @rdname predicate-utils
isEmpty <- function(x) {
  length(x) == 0
}
#' @rdname predicate-utils
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
#' Type predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-type
#' @keywords internal
NULL

#' @rdname predicate-type
isList <- function(x) {
  typeof(x) == "list"
}
#' @rdname predicate-type
isAtomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double",
                   "complex", "character", "raw")
}
#' @rdname predicate-type
isVector <- function(x) {
  isAtomic(x) || isList(x)
}
#' @rdname predicate-type
isNumeric <- function(x) {
  typeof(x) %in% c("integer", "double")
}
#' @rdname predicate-type
isInteger <- function(x) {
  typeof(x) == "integer"
}
#' @rdname predicate-type
isDouble <- function(x) {
  typeof(x) == "double"
}
#' @rdname predicate-type
isCharacter <- function(x) {
  typeof(x) == "character"
}
#' @rdname predicate-type
isLogical <- function(x) {
  typeof(x) == "logical"
}
#' @rdname predicate-type
isError <- function(x) {
  inherits(x, "try-error")
}

# ==============================================================================
#' Scalar type predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-scalar
#' @keywords internal
NULL

#' @name predicate-scalar
isScalarList <- function(x) {
  isList(x) && length(x) == 1
}
#' @name predicate-scalar
isScalarAtomic <- function(x) {
  isAtomic(x) && length(x) == 1
}
#' @name predicate-scalar
isScalarVector <- function(x) {
  isVector(x) && length(x) == 1
}
#' @name predicate-scalar
isScalarNumeric <- function(x) {
  isNumeric(x) && length(x) == 1
}
#' @name predicate-scalar
isScalarInteger <- function(x) {
  isInteger(x) && length(x) == 1
}
#' @name predicate-scalar
isScalarDouble <- function(x) {
  isDouble(x) && length(x) == 1
}
#' @name predicate-scalar
isScalarCharacter <- function(x) {
  isCharacter(x) && length(x) == 1
}
#' @name predicate-scalar
isScalarLogical <- function(x) {
  isLogical(x) && length(x) == 1
}

# ==============================================================================
#' Numeric predicates
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
NULL

#' @rdname predicate-numeric
isOdd <- function(x) {
  as.logical(x %% 2)
}
#' @rdname predicate-numeric
isBinary <- function(x) {
  if (!isNumeric(x))
    stop(sprintf("`x` must be a numeric vector, not %.", typeof(x)))
  all(x %in% c(0, 1))
}
#' @rdname predicate-numeric
isEqual <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  # Validation
  if (!isNumeric(x))
    stop(sprintf("`x` must be a numeric vector, not %.", typeof(x)))
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
}
#' @rdname predicate-numeric
isIncreasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  all(x == cummax(x), na.rm = na.rm) && sum(x, na.rm = na.rm) != 0
}
#' @rdname predicate-numeric
isDecreasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  all(x == cummin(x), na.rm = na.rm) && sum(x, na.rm = na.rm) != 0
}
#' @rdname predicate-numeric
isOverlapping <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("`x` and `y` must be numeric vectors.")

  min(x) <= max(y) && max(x) >= min(y)
}
#' @rdname predicate-numeric
isPositive <- function(x, strict = FALSE, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (strict) all(x > 0, na.rm = na.rm) else all(x >= 0, na.rm = na.rm)
}
#' @rdname predicate-numeric
isWholeNumber <- function(x, tolerance = .Machine$double.eps^0.5) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  abs(x - round(x, digits = 0)) <= tolerance
}

# ==============================================================================
#' Matrix predicates
#'
#' \code{isSquare} checks if a matrix is square.
#'
#' \code{isSymmetric} checks if a matrix is symmetric.
#' @param x A \code{\link{matrix}} to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-matrix
#' @keywords internal
NULL

#' @rdname predicate-matrix
isSquare <- function(x) {
  if (is.matrix(x)) nrow(x) == ncol(x) else FALSE
}
#' @rdname predicate-matrix
isSymmetric <- function(x) {
  if (is.matrix(x)) identical(x, t(x)) else FALSE
}
