# HELPERS

#' Independance
#'
#' @param x A \eqn{m \times p}{m x p} \code{\link{numeric}} matrix.
#' @param method A \code{\link{character}} string giving the method to be used.
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
#' @return A \eqn{m \times p}{m x p} \code{\link{numeric}} matrix.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
independance <- function(x, method = c("EPPM", "PVI")) {
  # Validation
  method <- match.arg(method, several.ok = FALSE)
  if (!is.matrix(x) || !is.numeric(x))
    stop("A numeric matrix is expected.", call. = FALSE)

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
  check_type(n, expected = "numeric")
  check_type(k, expected = "numeric")

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
#' @return A list with the following elements:
#'  \describe{
#'   \item{values}{The \eqn{n} leave-one-out values.}
#'   \item{bias}{The jackknife estimate of bias.}
#'   \item{error}{he jackknife estimate of standard error.}
#'  }
#' @keywords internal
#' @noRd
jackknife <- function(x, do, ...) {
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
  jack_bias <- (n - 1) * (mean(jack_values) - hat)
  jack_error <- sqrt(((n - 1) / n) * sum((jack_values - mean(jack_values))^2))

  list(values = jack_values, bias = jack_bias, error = jack_error)
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
  check_type(x, expected = "numeric")
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

# confidence_mean <- function(x, alpha = 0.05, type = c("student", "normal")) {
#   # Validation
#   check_type(x, expected = "numeric")
#   type <- match.arg(type, several.ok = FALSE)
#
#   n <- length(x)
#   z <- switch(
#     type,
#     "normal" = stats::qnorm(1 - alpha / 2),
#     "student" = stats::qt(1 - alpha / 2, n - 1),
#     stop(sprintf("There is no such type: %s", type), call. = FALSE)
#   )
#   stardard_error <- sd(x) / sqrt(n)
#
#   margin <- z * stardard_error
#   mean(x) + margin * c(-1, 1)
# }

#' Bootstrap Confidence Interval
#'
#'
#' @return A \code{\link{numeric}} vector giving the margin of errors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
# confidence_bootstrap <- function(x, alpha = 0.05, n = 1000) {
#   # Validation
#   check_type(x, expected = "numeric")
#
#   # Bootstrap
#   x_names <- names(x)
#   sample_levels <- if (is.null(x_names)) seq_len(length(x)) else x_names
#   # Preserve original order of columns
#   sample_levels <- factor(sample_levels, levels = unique(sample_levels))
#   sample_seq <- rep(sample_levels, times = x)
#   sample_size <- length(sample_seq)
#
#   foo <- function(x, s) {
#     table(sample(x = x, size = s, replace = TRUE))
#   }
#   rpl <- replicate(n, foo(x = sample_seq, s = sample_size))# / sample_size)
#
#   apply(
#     X = rpl,
#     MARGIN = 1,
#     FUN = confidence_mean,
#     alpha = alpha
#   )
# }
