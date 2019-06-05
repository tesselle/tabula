# HELPERS

#' Helpers
#'
#' \code{compact} removes elements from a list or vector.
#' \code{detect} xxx.
#'
#' %o% allows for function composition.
#'
#' %||% xxx.
#' @param x An object.
#' @param f,f1,f2 A \code{\link{function}}. In \code{compact} and \code{detect}
#'  \code{f} must be a logical predicate.
#' @param g A \code{\link{function}}.
#' @param lhs An object.
#' @param rhs An object.
#' @details
#'  Adapted from H. Wickham's \emph{Avanced R}.
#' @references
#'  Wickham, H. (2014). \emph{Advanced R}. London: Chapman & Hall. The R Series.
#' @examples
#' \dontrun{
#' compact(is.null, list("A", NULL, "B"))
#' detect(is.na, c(1, 2, NA, 4, 5, NA))
#' count(is.na, c(1, 2, NA, 4, 5, NA))
#'
#' (mean %o% range)(1:5)
#'
#' NULL %||% 1
#' 0 %||% 1
#' }
#' @name helpers
#' @keywords internal
NULL

#' @rdname helpers
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}
#' @rdname helpers
`%o%` <- function(f, g) {
  function(...) f(g(...))
}
#' @rdname helpers
compact <- function(f, x) {
  Filter(Negate(f), x)
}
#' @rdname helpers
detect <- function(f, x) {
  vapply(x, f, logical(1))
}
#' @rdname helpers
count <- function(f, x) {
  sum(detect(f, x))
}

#' Indices of a rolling window
#'
#' @param x An object.
#' @param window A \code{\link{integer}} scalar giving the window size.
#' @return A \code{\link{list}} with the following components:
#'  \describe{
#'   \item{i}{A \code{\link{integer}} vector of indices.}
#'   \item{w}{A \code{\link{integer}} vector of indices giving the window
#'   number.}
#'   \item{m}{A \code{\link{integer}} vector of indices giving the indice of
#'   the window mid-point.}
#'  }
#' @keywords internal
roll <- function(x, window = 3, simplify = FALSE) {
  # Validation
  if (!isOdd(window))
    stop("`window` must be an odd integer.", call. = FALSE)

  if (is.matrix(x) || is.data.frame(x)) {
    n <- nrow(x)
  } else {
    n <- length(x)
  }
  i <- seq_len(n) # Indices of the rows
  # Matrix of rolling-window indices of length w
  w <- stats::embed(i, window)[, window:1]
  inds <- i[c(t(w))] # Flatten indices
  # Window mid-point
  m <- w[, ceiling(window / 2)]
  list(i = inds, w = rep(m, each = window))
}

#' UUID v4
#'
#' Generates a universally unique identifier (UUID v4).
#' @param seed A single \code{\link{integer}} specifying the seeds.
#'  If \code{NULL} (the default) the seed will be re-initialized.
#' @details
#'  As it relies on R's internal random number generators and so will suffer
#'  from the use of \code{\link{set.seed}} in a session, the seed is
#'  re-initialized during execution (unless \code{seed} is not \code{NULL}).
#'  To prevent any side effects, the random number generator (RNG) state is
#'  saved and restored when the function exits.
#' @return A 36 characters long \code{\link{character}} string.
#' @seealso \link{set.seed}
#' @author N. Frerebeau
#' @name UUID
#' @keywords internal
NULL

#' @rdname UUID
generateUUID <- function(seed = NULL) {
  # Save and restore the random number generator (RNG) state
  if (!exists(".Random.seed", mode = "numeric")) sample(NA)
  old_seed <- .Random.seed
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  # Set seed
  seed <- if (is.numeric(seed)) seed else NULL
  set.seed(seed = seed)

  # Generate 32 pseudo random hex digits
  hex_digits <- c(as.character(0:9), letters[seq_len(6)])
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
  uuid
}

#' @rdname UUID
checkUUID <- function(x) {
  arg <- deparse(substitute(x))

  if (!isScalarCharacter(x) || is.na(x))
    throwError(arg, must = "be a character string.")

  n <- nchar(x)
  if (n != 36) {
    throwError(arg, must = "be a 36 characters long string", not = n)
  }
}

#' @rdname UUID
compareUUID <- function(x, y) {
  checkUUID(x)
  checkUUID(y)

  if (x != y) {
    stop(sprintf("IDs do not match:\n* %s\n* %s", x, y), call. = FALSE)
  }
}
