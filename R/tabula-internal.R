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

#' Circle
#'
#' Draws a circle.
#' @param x,y A length-one [`numeric`] vector giving the coordinates of the
#'  center of the circle.
#' @param radius A length-one [`numeric`] vector giving the radius of the
#'  circle.
#' @param n A length-on [`integer`] vector specifying the number of vertices to
#'  draw the circle.
#' @param ... Further parameters to be passed to [graphics::polygon()].
#' @return
#'  `circle()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @example inst/examples/ex-circle.R
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
circle <- function(x, y, radius, ..., n = 100) {
  angle.inc <- 2 * pi / n
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)

  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius + y
  graphics::polygon(xv, yv, ...)
}

#' Label Percentages
#'
#' @param x A [`numeric`] vector.
#' @param digits An [`integer`] indicating the number of decimal places.
#'  If `NULL` (the default), breaks will have the minimum number of digits
#'  needed to show the difference between adjacent values.
#' @param trim A [`logical`] scalar. If `FALSE` (the default), values are
#'  right-justified to a common width (see [base::format()]).
#' @return A [`character`] vector.
#' @keywords internal
#' @noRd
label_percent <- function(x, digits = NULL, trim = FALSE) {
  i <- !is.na(x)
  y <- x[i]
  y <- abs(y) * 100
  y <- format(y, trim = trim, digits = digits)
  y <- paste0(y, "%")
  x[i] <- y
  x
}
