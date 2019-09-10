# PREDICATES

#' Utility Predicates
#'
#' \code{is_empty} checks if a vector or list is empty.
#'
#' \code{is_subset} checks if a vector is a strict subset of another one.
#'
#' \code{is_named} checks if an object is named.
#' @param x,y,subset,set An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-utils
#' @keywords internal
#' @noRd
is_empty <- function(x) {
  length(x) == 0
}
is_subset <- function(subset, set) {
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
is_list <- function(x) {
  typeof(x) == "list"
}
is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double",
                   "complex", "character", "raw")
}
is_vector <- function(x) {
  is_atomic(x) || is_list(x)
}
is_numeric <- function(x) {
  typeof(x) %in% c("integer", "double")
}
is_integer <- function(x) {
  typeof(x) == "integer"
}
is_double <- function(x) {
  typeof(x) == "double"
}
is_character <- function(x) {
  typeof(x) == "character"
}
is_logical <- function(x) {
  typeof(x) == "logical"
}
is_error <- function(x) {
  inherits(x, "try-error") || inherits(x, "error")
}

# ==============================================================================
#' Scalar Type Predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-scalar
#' @keywords internal
#' @noRd
is_scalar_list <- function(x) {
  is_list(x) && length(x) == 1
}
is_scalar_atomic <- function(x) {
  is_atomic(x) && length(x) == 1
}
is_scalar_vector <- function(x) {
  is_vector(x) && length(x) == 1
}
is_scalar_numeric <- function(x) {
  is_numeric(x) && length(x) == 1
}
is_scalar_integer <- function(x) {
  is_integer(x) && length(x) == 1
}
is_scalar_double <- function(x) {
  is_double(x) && length(x) == 1
}
is_scalar_character <- function(x) {
  is_character(x) && length(x) == 1
}
is_scalar_logical <- function(x) {
  is_logical(x) && length(x) == 1
}

# ==============================================================================
#' Numeric Predicates
#'
#' Check numeric objects:
#'
#' \code{is_odd} checks if a number is odd.
#'
#' \code{is_binary} checks if an object contains only \eqn{0}s and \eqn{1}s.
#'
#' \code{is_equal} checks for equality among all elements of a vector.
#'
#' \code{is_increasing} and \code{is_decreasing} check if a sequence of numbers
#' is monotonically increasing or decreasing.
#'
#' \code{is_overlapping} checks if two data ranges overlap at all.
#'
#' \code{is_positive} checks if an object only contains positive values.
#'
#' \code{is_whole} checks if an object only contains whole numbers.
#' @param x,y A \code{\link{numeric}} object to be tested.
#' @param tolerance A \code{\link{numeric}} scalar giving the
#'  tolerance to check within.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted?
#' @return A \code{\link{logical}} scalar.
#' @name predicate-numeric
#' @keywords internal
#' @noRd
is_odd <- function(x) {
  as.logical(x %% 2)
}
is_positive <- function(x, strict = FALSE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  if (strict) x > 0 else x >= 0
}
is_whole <- function(x, tolerance = .Machine$double.eps^0.5) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  abs(x - round(x, digits = 0)) <= tolerance
}
is_binary <- function(x) {
  if (!is.numeric(x))
    stop(sprintf("`x` must be a numeric vector, not %.", typeof(x)))
  x %in% c(0, 1)
}
is_equal <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop(sprintf("`x` must be a numeric vector, not %.", typeof(x)))
  k <- abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
  if (is.na(k)) k <- FALSE
  k
}
is_increasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  k <- all(x == cummax(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}
is_decreasing <- function(x, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  k <- all(x == cummin(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}
is_overlapping <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("`x` and `y` must be numeric vectors.")
  min(x) <= max(y) && max(x) >= min(y)
}

# ==============================================================================
#' Matrix Predicates
#'
#' \code{is_square} checks if a matrix is square.
#'
#' \code{is_symmetric} checks if a matrix is symmetric.
#' @param x A \code{\link{matrix}} to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-matrix
#' @keywords internal
#' @noRd
is_square <- function(x) {
  if (is.matrix(x)) nrow(x) == ncol(x) else FALSE
}
is_symmetric <- function(x) {
  if (is.matrix(x)) identical(x, t(x)) else FALSE
}

# ==============================================================================
#' UUID
#'
#' Checks if a string is a canonically formatted UUID that is Version 1 through
#'  5 and is the appropriate Variant as per RFC4122.
#' @param x A \code{\link{matrix}} to be tested.
#' @return A \code{\link{logical}} scalar.
#' @name predicate-uuid
#' @keywords internal
#' @noRd
is_uuid <- function(x) {
  pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  grepl(pattern, x)
}
