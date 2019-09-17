# HELPERS

#' Helpers
#'
#' \code{compact} removes elements from a list or vector.
#' \code{detect} xxx.
#' \code{count} xxx.
#' \code{extract} extracts string form another string based on a pattern.
#'
#' %o% allows for function composition.
#' %||% allows to define a default value.
#' @param x,y An object.
#' @param f,g A \code{\link{function}}. In \code{compact}, \code{detect}
#'  and \code{count} \code{f} must be a logical predicate.
#' @param pattern A \code{\link{character}} string containing a regular
#'  expression.
#' @references
#'  Wickham, H. (2014). \emph{Advanced R}. London: Chapman & Hall. The R Series.
#' @keywords internal utilities
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x) || length(x) != 0) x else y
}
`%o%` <- function(f, g) {
  function(...) f(g(...))
}
compact <- function(f, x) {
  Filter(Negate(f), x)
}
detect <- function(f, x) {
  vapply(x, f, logical(1))
}
count <- function(f, x) {
  sum(detect(f, x))
}
extract <- function(x, pattern) {
  regmatches(x, regexpr(pattern, x))
}

#' Row Names
#'
#' Converts row names to an explicit column.
#' @param x A \code{\link{matrix}} or \code{\link{data.frame}}.
#' @param factor A \code{\link{logical}} scalar: should row names be coerced to
#'  factors? The default (\code{TRUE}) preserves the original ordering of the
#'  columns.
#' @param id A \code{\link{character}} string giving the name of the newly
#'  created column.
#' @return A data.frame
#' @author N. Frerebeau
#' @keywords internal utilities
#' @noRd
rownames_to_column <- function(x, factor = TRUE, id = "id") {
  if (!is.matrix(x) && !is.data.frame(x))
    stop("A matrix or data.frame is expected.", call. = FALSE)

  row_names <- rownames(x)
  col_names <- colnames(x)
  if (is.null(row_names)) {
    row_names <- seq_len(nrow(x))
  }
  if (is.null(col_names)) {
    col_names <- paste0("V", seq_len(ncol(x)))
  }
  if (factor) {
    row_names <- factor(x = row_names, levels = row_names)
  }
  y <- cbind.data.frame(row_names, x, stringsAsFactors = FALSE)
  if (is.character(id)) {
    colnames(y) <- c(id, col_names)
  }
  y
}

#' UUID v4
#'
#' Generates a universally unique identifier (UUID v4).
#' @param x,y A \code{\link{character}} string (UUID).
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
#' @keywords internal utilities
#' @noRd

generate_uuid <- function(seed = NULL) {
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

compare_uuid <- function(x, y) {
  check_uuid(x)
  check_uuid(y)

  if (x != y) {
    stop(sprintf("UUIDs do not match:\n* %s\n* %s", x, y), call. = FALSE)
  }
}
