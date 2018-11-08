#' @include AllGenerics.R
NULL

# Threshold
#
# @param object A \eqn{m \times p}{m x p} \link{\code{numeric}} matrix.
# @param method A \link{\code{character}} string giving the method to be used.
#  This must be one of "\code{EPPM}" or "\code{PVI}" (see details). Any
#  unambiguous substring can be given.
# @details
#  Computes for each cell of a numeric matrix one of the following thresholds :
#  \describe{
#   \item{EPPM}{The positive deviation from the column mean percentage (in
#    french "écart positif au pourcentage moyen", EPPM)}
#   \item{PVI}{The percentage of independence value (in french,
#   "pourcentage de valeur d'indépendance", PVI).}
#  }
# @return A \eqn{m \times p}{m x p} \link{\code{numeric}} matrix.
# @author N. Frerebeau
threshold <- function(object, method = c("EPPM", "PVI")) {
  # Validation -----------------------------------------------------------------
  method <- match.arg(method, several.ok = FALSE)

  # Independance ---------------------------------------------------------------
  indep <- apply(
    X = object, MARGIN = 1, FUN = function(object, column_total, grand_total) {
      sum(object) * column_total / grand_total
    },
    column_total = colSums(object),
    grand_total = sum(object)
  )
  # Threshold ------------------------------------------------------------------
  if (method == "EPPM") {
    threshold <- object - t(indep)
    threshold[threshold < 0] <- 0
  }
  if (method == "PVI") {
    threshold <- object / t(indep)
  }

  dimnames(threshold) <- dimnames(object)
  return(threshold)
}

# Binomial coefficient
#
# Computes the number of \code{k}-combinations from a given set of \code{n}
#  elements ("\code{n} choose \code{k}").
# @param n A length-one \code{\link{numeric}} vector.
# @param k A length-one \code{\link{numeric}} vector.
# @details
#  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
# @return A length-one \code{\link{numeric}} vector.
# @references
#  Ramanujan Aiyangar, S. (1988). \emph{The lost notebook and other unpublished
#  papers}. Berlin: Springer-Verlag.
# @author N. Frerebeau
combination <- function(n, k) {
  # Validation -----------------------------------------------------------------
  if (k > n) { stop("k cannot be larger than n") }
  # Ramanujan factorial approximation ------------------------------------------
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

# Confidence interval for a proportion
#
# Computes a confidence interval for a porportion at a desired level of
#  significance.
# @param x A \code{\link{numeric}} vector.
# @param level A length-one \code{\link{numeric}} vector giving the significance
#  level to be used.
# @return A \code{\link{numeric}} vector.
# @author N. Frerebeau
confidence <- function(x, level = 0.05) {
  z <- qnorm(1 - level / 2)
  n <- sum(x)
  p <- x / n
  stardard_error <- sqrt(p * (1 - p) / n)
  return(z * stardard_error)
}

