# HELPERS

is_incidence <- function(x) {
  if (is.logical(x)) return(TRUE)
  x <- as.numeric(x)
  all(x == 0 | x == 1, na.rm = TRUE)
}

#' Rolling Sum
#'
#' @param x A [`numeric`] vector.
#' @param n An [`integer`] giving the rolling window size.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
roll_sum <- function(x, n = 2) {
  utils::tail(cumsum(x) - cumsum(c(rep(0, n), utils::head(x, -n))), -n + 1)
}

#' Plotting Dimensions of Character Strings
#'
#' Convert string length in inch to number of (margin) lines.
#' @param x A [`character`] vector of string whose length is to be calculated.
#' @param ... Further parameter to be passed to [graphics::strwidth()]`, such as
#'  `cex`.
#' @return
#'  A [`numeric`] vector (maximum string width in units of margin lines).
#' @note For internal use only.
#' @family graphic tools
#' @keywords internal
#' @noRd
inch2line <- function(x, ...) {
  (max(graphics::strwidth(x, units = "inch", ...)) /
     graphics::par("cin")[2] + graphics::par("mgp")[2]) * graphics::par("cex")
}
