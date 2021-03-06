# STATISTICS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname independance
#' @aliases eppm,CountMatrix-method
setMethod(
  f = "eppm",
  signature = signature(object = "CountMatrix"),
  definition = function(object) {
    # Independance
    values <- apply(
      X = object, MARGIN = 1, FUN = function(x, column_total, grand_total) {
        sum(x) * column_total / grand_total
      },
      column_total = colSums(object),
      grand_total = sum(object)
    )
    # Threshold
    threshold <- (object - t(values)) / rowSums(object)
    threshold[threshold < 0] <- 0

    dimnames(threshold) <- dimnames(object)
    threshold
  }
)

#' @export
#' @rdname independance
#' @aliases pvi,CountMatrix-method
setMethod(
  f = "pvi",
  signature = signature(object = "CountMatrix"),
  definition = function(object) {
    # Independance
    values <- apply(
      X = object, MARGIN = 1, FUN = function(x, column_total, grand_total) {
        sum(x) * column_total / grand_total
      },
      column_total = colSums(object),
      grand_total = sum(object)
    )
    # Threshold
    threshold <- object / t(values)

    dimnames(threshold) <- dimnames(object)
    threshold
  }
)

#' Binomial Coefficient
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
#' @keywords internal
#' @noRd
combination <- function(n, k) {
  # Validation
  if (!is.numeric(n))
    stop("`n` must be a numeric vector.")
  if (!is.numeric(k))
    stop("`k` must be a numeric vector.")

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
  c
}

#' Jackknife Estimation
#'
#' @param x A vector.
#' @param do A \code{\link{function}} that takes \code{x} as an argument
#'  and returns a single numeric value.
#' @param ... Extra arguments passed to \code{do}.
#' @return A \code{numeric} vector with the following elements:
#'  \describe{
#'   \item{values}{The \eqn{n} leave-one-out values.}
#'   \item{mean}{The jackknife estimate of mean.}
#'   \item{bias}{The jackknife estimate of bias.}
#'   \item{error}{he jackknife estimate of standard error.}
#'  }
#' @keywords internal
stats_jackknife <- function(x, do, ...) {
  n <- length(x)
  hat <- do(x, ...)

  jack_values <- vapply(
    X = seq_len(n),
    FUN = function(i, x, do, ...) {
      do(x[-i], ...)
    },
    FUN.VALUE = double(1),
    x, do, ...
  )

  jack_mean <- mean(jack_values)
  jack_bias <- (n - 1) * (jack_mean - hat)
  jack_error <- sqrt(((n - 1) / n) * sum((jack_values - jack_mean)^2))

  results <- c(jack_mean, jack_bias, jack_error)
  names(results) <- c("mean", "bias", "error")
  results
}

#' Bootstrap Estimation
#'
#' @param x A vector.
#' @param do A \code{\link{function}} that takes \code{x} as an argument
#'  and returns a single numeric value.
#' @param probs A \code{\link{numeric}} vector of probabilities with values in
#'  \eqn{[0,1]} (see \code{\link[stats:quantile]{quantile}}).
#' @param n A non-negative \code{\link{integer}} giving the number of bootstrap
#' replications.
#' @param na.rm A \code{\link{logical}} scalar: should missing values be removed
#' from \code{x} before the quantiles are computed?
#' @param ... Extra arguments passed to \code{do}.
#' @return A \code{numeric} vector with the following elements:
#'  \describe{
#'   \item{min}{Minimum value.}
#'   \item{mean}{Mean value.}
#'   \item{max}{Maximum value.}
#'   \item{Q*}{Sample quantile to * probability.}
#'  }
#' @keywords internal
stats_bootstrap <- function(x, do, probs = c(0.05, 0.95),
                            n = 1000, na.rm = FALSE, ...) {
  total <- sum(x)
  replicates <- stats::rmultinom(n, size = total, prob = x / total)
  boot_values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)

  Q <- stats::quantile(boot_values, probs = probs, na.rm = na.rm, names = FALSE)
  quant <- paste0("Q", round(probs * 100, 0))

  results <- c(
    min(boot_values, na.rm = na.rm),
    mean(boot_values, na.rm = na.rm),
    max(boot_values, na.rm = na.rm),
    Q
  )
  names(results) <- c("min", "mean", "max", quant)
  results
}

#' Confidence Interval for a Proportion
#'
#' Computes the margin of errors of a confidence interval at a desired level of
#'  significance.
#' @param x A \code{\link{numeric}} vector.
#' @param alpha A length-one \code{\link{numeric}} vector giving the significance
#'  level to be used.
#' @param type A \code{\link{character}} string giving the type of confidence
#'  interval to be returned. It must be one \code{normal} (default) or
#'  \code{student}. Any unambiguous substring can be given.
#' @return A \code{\link{numeric}} vector giving the margin of errors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
confidence_proportion <- function(x, alpha = 0.05,
                                  type = c("normal", "student")) {
  # Validation
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.")
  type <- match.arg(type, several.ok = FALSE)

  n <- sum(x)
  p <- x / n
  z <- switch(
    type,
    "normal" = stats::qnorm(1 - alpha / 2),
    "student" = stats::qt(1 - alpha / 2, n - 1),
    stop(sprintf("There is no such type: %s", type), call. = FALSE)
  )
  stardard_error <- sqrt(p * (1 - p) / n)

  margin <- z * stardard_error
  margin
}
