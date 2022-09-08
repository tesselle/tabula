# STATISTICS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname resample
#' @aliases resample,numeric-method
setMethod(
  f = "resample",
  signature = c(object = "numeric"),
  definition = function(object, do, n, size = sum(object), ..., f = NULL) {
    ## Validation
    arkhe::assert_count(object)

    prob <- object / sum(object)
    replicates <- stats::rmultinom(n, size = size, prob = prob)
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    if (is.function(f)) values <- f(values)
    values
  }
)

expected <- function(x) {
  m <- nrow(x)
  p <- ncol(x)

  column_total <- matrix(colSums(x), nrow = m, ncol = p, byrow = TRUE)
  row_total <- matrix(rowSums(x), nrow = m, ncol = p, byrow = FALSE)
  grand_total <- sum(x)

  column_total * row_total / grand_total
}

#' @export
#' @rdname independance
#' @aliases eppm,matrix-method
setMethod(
  f = "eppm",
  signature = signature(object = "matrix"),
  definition = function(object) {
    # Independance
    values <- expected(object)

    # Threshold
    threshold <- (object - values) / rowSums(object)
    threshold[threshold < 0] <- 0

    dimnames(threshold) <- dimnames(object)
    threshold
  }
)

#' @export
#' @rdname independance
#' @aliases eppm,data.frame-method
setMethod(
  f = "eppm",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

#' @export
#' @rdname independance
#' @aliases pvi,matrix-method
setMethod(
  f = "pvi",
  signature = signature(object = "matrix"),
  definition = function(object) {
    # Independance
    values <- expected(object)

    # Threshold
    threshold <- object / values

    dimnames(threshold) <- dimnames(object)
    threshold
  }
)

#' @export
#' @rdname independance
#' @aliases pvi,data.frame-method
setMethod(
  f = "pvi",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

#' Binomial Coefficient
#'
#' Computes the number of `k`-combinations from a given set of `n` elements
#' ("`n` choose `k`").
#' @param n A length-one [`numeric`] vector.
#' @param k A length-one [`numeric`] vector.
#' @details
#'  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
#' @return A length-one [`numeric`] vector.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
combination <- function(n, k) {
  # Validation
  if (!is.numeric(n))
    stop("`n` must be a numeric vector.")
  if (!is.numeric(k))
    stop("`k` must be a numeric vector.")

  if (n > 170 | k > 170) {
    if (getOption("tabula.verbose")) message("Ramanujan approximation of x!")
    c <- exp(ramanujan(n) - ramanujan(k) - ramanujan(n - k))
  } else {
    c <- factorial(n) / (factorial(k) * factorial(n - k))
  }
  c
}

#' Ramanujan Factorial Approximation
#'
#' @param x A [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @examples
#'  factorial(50)
#'  exp(ramanujan(50))
#' @references
#'  Ramanujan Aiyangar, S. (1988). *The lost notebook and other unpublished
#'  papers*. Berlin: Springer-Verlag.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
ramanujan <- function(x){
  x * log(x) - x + log(x * (1 + 4 * x * (1 + 2 * x))) / 6 + log(pi) / 2
}
