# HELPERS

#' Helpers
#'
#' \code{compact} removes elements from a list or vector.
#' \code{detect} finds values in a list or vector according to a given
#' predicate.
#' \code{count} counts values in a list or vector according to a given
#' predicate.
#' \code{extract} extracts a string form another string based on a pattern.
#'
#' \code{\%o\%} allows for function composition.
#' \code{\%||\%} allows to define a default value.
#' @param x,y An object.
#' @param f,g A \code{\link{function}}. In \code{compact}, \code{detect}
#'  and \code{count} \code{f} must be a \code{\link{logical}} predicate.
#' @param pattern A \code{\link{character}} string containing a regular
#'  expression.
#' @references
#'  Wickham, H. (2014). \emph{Advanced R}. London: Chapman & Hall. The R Series.
#' @family utilities
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

#' Scales
#'
#' @param x A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
scale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
scale_pc <- function(x) {
  paste0(round(x = abs(x) * 100, digits = 0), "%")
}

#' Rolling Sum
#'
#' @param x A \code{\link{numeric}} vector.
#' @param n An \code{\link{integer}} giving the rolling window size.
#' @return A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
roll_sum <- function(x, n = 2) {
  utils::tail(cumsum(x) - cumsum(c(rep(0, n), utils::head(x, -n))), -n + 1)
}

#' Indices of a rolling window
#'
#' @param x An object.
#' @param window A \code{\link{integer}} scalar giving the window size.
#' @return A \code{\link{list}} with the following components:
#'  \describe{
#'   \item{i}{A \code{\link{integer}} vector of indices.}
#'   \item{w}{A \code{\link{integer}} vector of indices giving the indice of
#'   the window mid-point.}
#'  }
#' @keywords internal
#' @noRd
roll <- function(x, window = 3) {
  ## Validation
  if (window %% 2 == 0)
    stop(sprintf("%s must be an odd integer.", sQuote("window")), call. = FALSE)

  n <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
  i <- seq_len(n) # Indices of the rows

  ## Matrix of rolling-window indices of length w
  w <- stats::embed(i, window)[, window:1]
  inds <- as.vector(t(w)) # Flatten indices

  ## Window mid-point
  m <- w[, ceiling(window / 2)]

  list(i = inds, w = rep(m, each = window))
}

#' Row and Column Names
#'
#' \code{rownames_to_column} converts row names to an explicit column.
#' @param x A \code{\link{matrix}} or \code{\link{data.frame}}.
#' @param factor A \code{\link{logical}} scalar: should row names be coerced to
#'  factors? The default (\code{TRUE}) preserves the original ordering of the
#'  columns.
#' @param id A \code{\link{character}} string giving the name of the newly
#'  created column.
#' @return A \code{\link{data.frame}}
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
rownames_to_column <- function(x, factor = TRUE, id = "id") {
  if (!is.matrix(x) && !is.data.frame(x))
    stop("A matrix or data.frame is expected.", call. = FALSE)

  if (is.null(colnames(x))) {
    colnames(x) <- paste0("col", seq_len(ncol(x)))
  }
  row_names <- rownames(x)
  if (is.null(row_names)) {
    row_names <- paste0("row", seq_len(nrow(x)))
  }
  if (factor) {
    row_names <- factor(x = row_names, levels = row_names)
  }

  z <- cbind.data.frame(row_names, x, stringsAsFactors = FALSE)
  colnames(z) <- c(id, colnames(x))
  rownames(z) <- NULL
  z
}

#' Subset
#'
#' Subset a matrix by matching names.
#' @param x A \code{\link{matrix}}.
#' @param y A named \code{\link{vector}}.
#' @return A \code{\link{matrix}}.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
subset_by_names <- function(x, y) {
  if (!is.null(names(y)) && !is.null(rownames(x))) {
    index <- match(names(y), rownames(x))
    if (length(index) < length(y)) {
      warning("Some names do not match.", call. = FALSE)
    }
    x <- x[index, , drop = FALSE]
  } else if (nrow(x) != length(y)) {
    x <- x[seq_along(y), ]
  }
  x
}
bind_by_names <- function(x, y) {
  if (!is.null(names(y)) && !is.null(rownames(x))) {
    z <- merge(
      x = data.frame(y),
      y = as.data.frame(x),
      by = 0,
      all.x = FALSE,
      all.y = TRUE,
      sort = FALSE
    )
    z <- z[, -c(1)] # Remove extra column from merge
  } else if (nrow(x) == length(y)) {
    z <- data.frame(y, x)
  } else {
    stop("Names are missing.", call. = FALSE)
  }
}

#' Build a Long Data Frame
#'
#' Stacks vector from a \code{\link{data.frame}}.
#' @param x A \code{\link{matrix}} or \code{\link{data.frame}}
#' @param value A \code{\link{character}} string specifying the name of the
#'  column containing the result of concatenating \code{x}.
#' @param factor A \code{\link{logical}} scalar: should row and columns names be
#'  coerced to factors? The default (\code{TRUE}) preserves the original
#'  ordering.
#' @return A \code{\link{data.frame}} withe the following variables:
#'  "\code{case}", "\code{value}" and "\code{type}".
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
wide2long <- function(x, value = "data", factor = TRUE) {
  x <- as.data.frame(x)
  row_names <- rownames(x)
  col_names <- rownames(x)

  stacked <- utils::stack(x)
  long <- cbind.data.frame(stacked, row_names)
  colnames(long) <- c(value, "type", "case")
  if (factor) {
    # Preserves the original ordering of the rows and columns
    long$case <- factor(long$case, levels = unique(long$case))
    long$type <- factor(long$type, levels = unique(long$type))
  }
  long
}
