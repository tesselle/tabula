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
