#' @include AllGenerics.R AllClasses.R
NULL

# Bootstrap ====================================================================
#' @export
#' @rdname refine
#' @aliases bootstrap,DiversityIndex-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "DiversityIndex"),
  definition = function(object, probs = c(0.05, 0.95), n = 1000) {
    results <- apply(
      X = object[["data"]],
      MARGIN = 1,
      FUN = stats_bootstrap,
      do = object[["index"]],
      probs = probs,
      n = n
    )
    results <- do.call(rbind, results)
    return(results)
  }
)

# Jackknife ====================================================================
#' @export
#' @rdname refine
#' @aliases jackknife,DiversityIndex-method
setMethod(
  f = "jackknife",
  signature = signature(object = "DiversityIndex"),
  definition = function(object) {
    results <- apply(
      X = object[["data"]],
      MARGIN = 1,
      FUN = function(x, do) stats_jackknife(x, do)[c("mean", "bias", "error")],
      do = object[["index"]]
    )
    results <- do.call(rbind, results)
    return(results)
  }
)

# Simulate =====================================================================
#' @export
#' @rdname refine
#' @aliases simulate,DiversityIndex-method
setMethod(
  f = "simulate",
  signature = signature(object = "DiversityIndex"),
  definition = function(object, quantiles = TRUE,
                        level = 0.80, step = 1, n = 1000,
                        progress = getOption("tabula.progress"), ...) {

    ## Get data
    x <- object[["data"]]
    ## Select method
    fun <- object[["index"]]

    ## Simulate
    ## Specify the probability for the classes
    prob <- colSums(x) / sum(x)

    ## Sample size
    size <- max(rowSums(x))
    sample_sizes <- seq(from = 1, to = size * 1.05, by = step)

    m <- length(sample_sizes)
    k <- seq_len(m)

    simulated <- vector(mode = "list", length = m)

    progress_bar <- interactive() && progress
    if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

    for (i in k) {
      simulated[[i]] <- sim(size = sample_sizes[[i]], prob = prob,
                            method = fun, n = n, level = level,
                            quantiles = quantiles, ...)
      if (progress_bar) utils::setTxtProgressBar(pbar, i)
    }

    if (progress_bar) close(pbar)

    simulated <- do.call(rbind, simulated)
    simulated <- cbind(size = sample_sizes, simulated)

    methods::initialize(object, simulation = simulated)
  }
)

#' @param x A \code{\link{numeric}} matrix.
#' @param method A \code{\link{function}}.
#' @param prob A length-\eqn{p} \code{\link{numeric}} vector giving the of
#'  probability of the \eqn{p} taxa/types (see below). If \code{NULL} (the
#'  default), probabilities are estimated from \code{x}.
#' @param quantiles A \code{\link{logical}} scalar: should sample quantiles
#'  be used as confidence interval? If \code{TRUE} (the default),
#'  sample quantiles are used as described in Kintigh (1989), else quantiles of
#'  the normal distribution are used.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param step An \code{\link{integer}}.
#' @param n A non-negative \code{\link{integer}} giving the number of bootstrap
#'  replications.
#' @param progress A \code{\link{logical}} scalar: should a progress bar be
#'  displayed?
#' @keywords internal
#' @noRd
sim <- function(size, prob, method, n = 1000, level = 0.80,
                quantiles = TRUE, ...) {

  replicates <- stats::rmultinom(n = n, size = size, prob = prob)
  R <- apply(X = replicates, MARGIN = 2, FUN = method, ...)

  if (quantiles) {
    ## Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(R, probs = c(k, k + level), na.rm = TRUE)
  } else {
    ## Confidence interval
    conf <- mean(R) + stats::sd(R) * stats::qnorm(level / 2) * c(-1, 1)
  }

  result <- c(mean(R), conf)
  names(result) <- c("mean", "lower", "upper")
  return(result)
}
