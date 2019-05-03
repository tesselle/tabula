# HELPERS

#' UUID v4
#'
#' Generates a universally unique identifier (UUID v4).
#' @param seed A single \code{\link{integer}} specifying the seeds.
#'  If \code{NULL} (the default) the seed will be re-initialized.
#' @details
#'  As it rely on R's internal random number generators and so will suffer
#'  from the use of \code{\link{set.seed}} in a session, the seed is
#'  re-initialized during execution (unless \code{seed} is not \code{NULL}).
#'  To prevent any side effects, the random number generator (RNG) state is
#'  saved and restored when the function exits.
#' @return A 36 characters long \code{\link{character}} string.
#' @seealso \link{set.seed}
#' @author N. Frerebeau
#' @keywords internal
generateUUID <- function(seed = NULL) {
  # Save and restore the random number generator (RNG) state
  if (!exists(".Random.seed", mode = "numeric")) sample(NA)
  old_seed <- .Random.seed
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()))
  # Set seed
  seed <- if (is.numeric(seed)) seed else NULL
  set.seed(seed = seed)

  # Generate 32 pseudo random hex digits
  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_32 <- sample(hex_digits, size = 32, replace = TRUE)
  # Set version (4) and variant (1)
  hex_32[13] <- 4
  hex_32[17] <- sample(c(8, 9, "a", "b"), size = 1)

  uuid <- paste(
    mapply(
      FUN = substr,
      start = c(1, 9, 13, 17, 21),
      stop = c(8, 12, 16, 20, 32),
      MoreArgs = list(x = paste0(hex_32, collapse = "")),
      SIMPLIFY = FALSE
    ),
    collapse = "-"
  )
  return(uuid)
}

#' Binary numbers
#'
#' Checks if an object only contains 0s and 1s.
#' @param x A \code{\link{numeric}} object to be checked.
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @keywords internal
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
#' @return A \code{\link{logical}} scalar.
#' @seealso \code{\link[base]{is.integer}}
#' @keywords internal
#' @noRd
isWholeNumber <- function(x, tolerance = .Machine$double.eps^0.5) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.")
  abs(x - round(x)) <= tolerance
}
