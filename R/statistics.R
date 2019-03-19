# HELPERS
NULL

#' Independance
#'
#' @param x A \eqn{m \times p}{m x p} \link{\code{numeric}} matrix.
#' @param method A \link{\code{character}} string giving the method to be used.
#'  This must be one of "\code{EPPM}" or "\code{PVI}" (see details). Any
#'  unambiguous substring can be given.
#' @details
#'  Computes for each cell of a numeric matrix one of the following thresholds:
#'  \describe{
#'   \item{EPPM}{The positive deviation from the column mean percentage (in
#'    french "écart positif au pourcentage moyen", EPPM)}
#'   \item{PVI}{The percentage of independence value (in french,
#'   "pourcentage de valeur d'indépendance", PVI).}
#'  }
#' @return A \eqn{m \times p}{m x p} \link{\code{numeric}} matrix.
#' @author N. Frerebeau
#' @noRd
independance <- function(x, method = c("EPPM", "PVI")) {
  # Validation
  method <- match.arg(method, several.ok = FALSE)
  if (!is.matrix(x) | !is.numeric(x))
    stop("A numeric matrix is expected.")

  # Independance
  values <- apply(
    X = x, MARGIN = 1, FUN = function(x, column_total, grand_total) {
      sum(x) * column_total / grand_total
    },
    column_total = colSums(x),
    grand_total = sum(x)
  )
  # Threshold
  if (method == "EPPM") {
    threshold <- (x - t(values)) / rowSums(x)
    threshold[threshold < 0] <- 0
  }
  if (method == "PVI") {
    threshold <- x / t(values)
  }

  dimnames(threshold) <- dimnames(x)
  return(threshold)
}

#' Binomial coefficient
#'
#' Computes the number of \code{k}-combinations from a given set of \code{n}
#'  elements ("\code{n} choose \code{k}").
#' @param n A length-one \code{\link{numeric}} vector.
#' @param k A length-one \code{\link{numeric}} vector.
#' @details
#'  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
#' @return A length-one \code{\link{numeric}} vector.
#' @references
#'  Ramanujan Aiyangar, S. (1988). \emph{The lost notebook and other unpublished
#'  papers}. Berlin: Springer-Verlag.
#' @author N. Frerebeau
#' @noRd
combination <- function(n, k) {
  # Validation
  if (!is.numeric(n) | !is.numeric(k))
    stop("Numeric values are expected.")
  # Ramanujan factorial approximation
  ramanujan <- function(x){
    x * log(x) - x + log(x * (1 + 4 * x * (1 + 2 * x))) / 6 + log(pi) / 2
  }

  if (n > 170 | k > 170) {
    if (getOption("verbose")) message("Ramanujan approximation of x!")
    c <- exp(ramanujan(n) - ramanujan(k) - ramanujan(n - k))
  } else {
    c <- factorial(n) / (factorial(k) * factorial(n - k))
  }
  return(c)
}

#' Confidence interval for a proportion
#'
#' Computes the margin of errors of a confidence interval at a desired level of
#'  significance.
#' @param x A \code{\link{numeric}} vector.
#' @param level A length-one \code{\link{numeric}} vector giving the significance
#'  level to be used.
#' @param type A \code{\link{character}} string giving the type of confidence
#'  interval to be returned. It must be one \code{normal} (default) or
#'  \code{student}. Any unambiguous substring can be given.
#' @return A \code{\link{numeric}} vector giving the margin of errors.
#' @author N. Frerebeau
#' @noRd
confidence <- function(x, level = 0.95, type = c("normal", "student")) {
  # Validation
  if (!is.vector(x) | !is.numeric(x))
    stop("A numeric vector is expected.")
  type <- match.arg(type, several.ok = FALSE)

  n <- sum(x)
  p <- x / n
  z <- switch(
    type,
    "normal" = stats::qnorm(1 - level / 2),
    "student" = stats::qt(1 - level / 2, n - 1)
  )
  stardard_error <- sqrt(p * (1 - p) / n)

  margin <- z * stardard_error
  return(margin)
}

#' Jackknife estimation
#'
#' @param x A vector.
#' @param do A \code{\link{function}} that takes \code{x} as an argument
#'  and returns a single numeric value.
#' @param ... Extra arguments passed to \code{do}.
#' @return A list with the following elements:
#'  \describe{
#'   \item{values}{The \code{n} leave-one-out values.}
#'   \item{bias}{The jackknife estimate of bias.}
#'   \item{error}{he jackknife estimate of standard error.}
#'  }
#' @author N. Frerebeau
#' @noRd
jackknife <- function(x, do, ...) {
  n <- length(x)
  hat <- do(x, ...)
  jack_values <- sapply(X = 1:n, FUN = function(i, x, do, ...) {
    do(x[-i], ...)
  }, x, do, ..., simplify = TRUE)
  jack_bias <- (n - 1) * (mean(jack_values) - hat)
  jack_error <- sqrt(((n - 1) / n) * sum((jack_values - mean(jack_values))^2))

  results <- list(values = jack_values, bias = jack_bias, error = jack_error)
  return(results)
}
