# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

is_incidence <- function(x) {
  x <- as.numeric(x)
  all(x == 0 | x == 1)
}

#' Plotting Dimensions of Character Strings
#'
#' Convert string length in inch to number of (margin) lines.
#' @param x A [`character`] vector of string whose length is to be calculated.
#' @param ... Further parameter to be passed to [graphics::strwidth()]`, such as
#'  `cex`
#' @return
#'  A [`numeric`] vector (maximum string width in units of margin lines).
#' @keywords internal
#' @noRd
inch2line <- function(x, ...) {
  (max(graphics::strwidth(x, units = "inch", ...)) /
     graphics::par("cin")[2] + graphics::par("mgp")[2]) * graphics::par("cex")
}

#' Draw a Circle
#'
#' @param x,y A length-one [`numeric`] vector giving the coordinates of the
#'  center of the circle.
#' @param radius A length-one [`numeric`] vector giving the radius of the
#'  circle.
#' @param n A length-on [`integer`] vector specifying the number of vertices to
#'  draw the circle.
#' @param ... Further parameters to be passed to [graphics::polygon()].
#' @examples
#' \dontrun{
#' plot(NA, xlim = c(-1, 1), ylim = c(-1, 1),
#'      axes = FALSE, ann = FALSE, asp = 1)
#' plot_circle(0, 0, 0.5)
#' }
#' @keywords internal
#' @author N. Frerebeau
#' @noRd
plot_circle <- function(x, y, radius, n = 100, ...) {
  angle.inc <- 2 * pi / n
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)

  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius + y
  graphics::polygon(xv, yv, ...)
}

#' Colour Ramp
#'
#' Provides a colour scheme that map a `numeric` vector to colours.
#' @param x A [`numeric`] vector to be mapped to colours.
#' @param palette A vector of colors.
#' @param from A length-two [`numeric`] vector specifying the input range.
#'  If `NULL`, is calculated from the range of `x`.
#' @param midpoint A [`numeric`] value specifying the midpoint of `x`. If not
#'  `NULL`, `x` will be rescaled to have the specified `midpoint`.
#' @return A [`character`] vector of colors.
#' @keywords internal
#' @noRd
color_ramp <- function(x, palette, from = range(x, na.rm = TRUE),
                       midpoint = NULL) {
  ## Rescale to 0-1
  to <- c(0, 1)
  if (!is.null(midpoint) && is.numeric(midpoint)) {
    extent <- 2 * max(abs(from - midpoint))
    z <- (x - midpoint) / extent * diff(to) + mean(to)
  } else {
    z <- scale_range(x, to = to, from = from)
  }

  col <- grDevices::colorRamp(palette)(z)
  col <- grDevices::rgb(col, maxColorValue = 255)
  col
}

#' Scales
#'
#' @param x A [`numeric`] vector.
#' @keywords internal
#' @noRd
scale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
scale_pc <- function(x) {
  i <- !is.na(x)
  x[i] <- paste0(round(x = abs(x[i]) * 100, digits = 0), "%")
  x
}
scale_range <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  (x - from[1]) / diff(from) * diff(to) + to[1]
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

#' Reshape
#'
#' Transforms a `matrix` to a long `data.frame`.
#' @param from An object to be coerced.
#' @param factor A [`logical`] scalar: should character string be
#'  coerced to [`factor`]? Default to `FALSE`, if `TRUE` the original ordering is
#'  preserved.
#' @param ... Currently not used.
#' @return A coerced object.
#' @keywords internal
#' @noRd
wide_to_long <- function(from, factor = FALSE) {
  row <- row(from, as.factor = TRUE)
  col <- col(from, as.factor = TRUE)
  x <- data.frame(
    row = as.vector(row),
    column = as.vector(col),
    x = as.numeric(col),
    y = as.vector(nrow(from) - as.numeric(row) + 1), # Reverse y for plotting
    z = as.vector(from),
    stringsAsFactors = FALSE
  )
  x
}
