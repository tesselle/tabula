# HELPERS

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
