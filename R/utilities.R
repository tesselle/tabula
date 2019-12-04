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

#' Row and Column Names
#'
#' \code{rownames_to_column} converts row names to an explicit column.
#' @param x A \code{\link{matrix}} or \code{\link{data.frame}}.
#' @param factor A \code{\link{logical}} scalar: should row names be coerced to
#'  factors? The default (\code{TRUE}) preserves the original ordering of the
#'  columns.
#' @param id A \code{\link{character}} string giving the name of the newly
#'  created column.
#' @return A data.frame
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
make_rownames <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x))
    stop("A matrix or data.frame is expected.", call. = FALSE)
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }
  x
}
make_colnames <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x))
    stop("A matrix or data.frame is expected.", call. = FALSE)
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
  }
  x
}
make_dimnames <- function(x) {
  x <- make_rownames(x)
  x <- make_colnames(x)
  x
}
rownames_to_column <- function(x, factor = TRUE, id = "id") {
  if (!is.matrix(x) && !is.data.frame(x))
    stop("A matrix or data.frame is expected.", call. = FALSE)

  x <- make_dimnames(x)

  row_names <- rownames(x)
  if (factor) {
    row_names <- factor(x = row_names, levels = row_names)
  }
  y <- cbind.data.frame(row_names, x, stringsAsFactors = FALSE)

  id <- if (is.null(id)) "id" else id[[1L]]
  colnames(y) <- c(id, colnames(x))
  y
}
