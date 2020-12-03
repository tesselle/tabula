#' @include AllGenerics.R AllClasses.R
NULL

#' @param x A \code{\link{numeric}} \code{\link{matrix}}.
#' @param method A \code{\link{function}}.
#' @param simulate A \code{\link{logical}} scalar.
#' @param quantiles A \code{\link{logical}} scalar: should sample quantiles
#'  be used as confidence interval? If \code{TRUE} (the default),
#'  sample quantiles are used as described in Kintigh (1989), else quantiles of
#'  the normal distribution are used. Only used if \code{simulate} is
#'  \code{TRUE}.
#' @param prob A length-\eqn{p} \code{\link{numeric}} vector giving the of
#'  probability of the \eqn{p} taxa/types (see below). If \code{NULL} (the
#'  default), probabilities are estimated from the whole dataset.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param step An \code{\link{integer}}.
#' @param n A non-negative \code{\link{integer}} giving the number of bootstrap
#' replications.
#' @param progress A \code{\link{logical}} scalar: should a progress bar be
#'  displayed?
#' @param ... Further parameters to be passed to \code{method}.
#' @return A \code{\link{list}}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
index_diversity <- function(x, method, simulate = FALSE, quantiles = TRUE,
                            prob = NULL, level = 0.80, step = 1, n = 1000,
                            progress = getOption("tabula.progress"), ...) {

  # Heterogeneity
  idx <- apply(X = x, MARGIN = 1, FUN = method, ...)
  names(idx) <- rownames(x)

  # Simulation
  sim <- matrix(0, 0, 0)
  if (simulate) {
    sim <- simulate_diversity(x, method, prob = prob, quantiles = quantiles,
                              level = level, step = step, n = n,
                              progress = progress)
  }

  .DiversityIndex(
    data = as.matrix(x),
    index = idx,
    size = as.integer(rowSums(x)),
    simulation = sim
  )
}


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
simulate_diversity <- function(x, method, prob = NULL, quantiles = TRUE,
                               level = 0.80, step = 1, n = 1000,
                               progress = getOption("tabula.progress"), ...) {
  # Specify the probability for the classes
  if (is.null(prob))
    prob <- colSums(x) / sum(x)
  # Sample size
  size <- max(rowSums(x))
  sample_sizes <- seq(from = 1, to = size * 1.05, by = step)

  m <- length(sample_sizes)
  k <- seq_len(m)

  simulated <- vector(mode = "list", length = m)

  progress_bar <- interactive() && progress
  if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

  for (i in k) {
    simulated[[i]] <- sim(size = sample_sizes[[i]], prob = prob,
                          method = method, n = n, level = level,
                          quantiles = quantiles, ...)
    if (progress_bar) utils::setTxtProgressBar(pbar, i)
  }

  if (progress_bar) close(pbar)

  simulated <- do.call(rbind, simulated)
  simulated <- cbind(size = sample_sizes, simulated)
  simulated
}

sim <- function(size, prob, method, n = 1000, level = 0.80,
                quantiles = TRUE, ...) {
  replicates <- stats::rmultinom(n = n, size = size, prob = prob)
  R <- apply(X = replicates, MARGIN = 2, FUN = method, ...)

  if (quantiles) {
    # Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(R, probs = c(k, k + level), na.rm = TRUE)
  } else {
    # Confidence interval
    conf <- mean(R) + stats::sd(R) * stats::qnorm(level / 2) * c(-1, 1)
  }

   result <- c(mean(R), conf)
   names(result) <- c("mean", "lower", "upper")
   result
}
