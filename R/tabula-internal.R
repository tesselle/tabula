# HELPERS

is_incidence <- function(x) {
  x <- as.numeric(x)
  all(x == 0 | x == 1)
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
