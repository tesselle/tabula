#' @include AllGenerics.R AllClasses.R
NULL

index_diversity <- function(x, method, jackknife = TRUE, bootstrap = TRUE,
                            simulate = FALSE, prob = NULL, level = 0.80,
                            n = 1000, ...) {
  jack <- boot <- sim <- matrix(0, 0, 0)
  # Heterogeneity
  idx <- apply(X = x, MARGIN = 1, FUN = method, ...)
  names(idx) <- rownames(x)
  # Jackknife
  if (jackknife) {
    jack <- apply(
      X = x,
      MARGIN = 1,
      FUN = function(x, foo, ...) {
        jackknife(x, foo, ...)[c("mean", "bias", "error")]
      },
      foo = method, ...
    )
    jack <- do.call(rbind, jack)
  }
  # Bootstrap
  if (bootstrap) {
    boot <- apply(
      X = x,
      MARGIN = 1,
      FUN = function(x, foo, n, ...) bootstrap(x, foo, n, ...),
      foo = method, n = n, ...
    )
    boot <- do.call(rbind, boot)
  }
  # Simulation
  if (simulate) {
    sim <- simulate_diversity(x, method, prob = prob, level = level, n = n)
  }

  .DiversityIndex(
    id = arkhe::get_id(x),
    index = idx,
    size = as.integer(rowSums(x)),
    jackknife = jack,
    boostrap = boot,
    simulated = sim
  )
}


#' @param x A \code{\link{numeric}} vector giving the sample size.
#' @param prob A length-\eqn{p} \code{\link{numeric}} vector giving the of
#'  probability of the \eqn{p} taxa/types (see below). If \code{NULL} (the
#'  default), probabilities are estimated from the whole dataset.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param n A non-negative \code{\link{integer}} giving the number of bootstrap
#' replications.
#' @keywords internal
#' @noRd
simulate_diversity <- function(x, method, prob = NULL,
                               level = 0.80, n = 1000, ...) {
  # Specify the probability for the classes
  if (is.null(prob) && is.matrix(x)) {
    prob <- colSums(x) / sum(x)
  }
  # Sample size
  if (is.matrix(x)) {
    size <- max(rowSums(x))
    sample_sizes <- seq_len(size * 1.05)
  } else {
    sample_sizes <- x
  }

  sim <- function(x, n, prob, level, method, ...) {
    replicates <- stats::rmultinom(n, size = x, prob = prob)
    R <- apply(X = replicates, MARGIN = 2, FUN = method, ...)

    # Confidence interval as described in Kintigh 1989
    # k <- (1 - level) / 2
    # conf <- quantile(R, probs = c(k, k + level))

    # Confidence interval
    conf <- mean(R) + stats::sd(R) * stats::qnorm(level / 2) * c(-1, 1)

    c(mean(R), conf)
  }
  loop_args <- list(X = sample_sizes, FUN = sim,
                    n = n, prob = prob, level = level, method = method, ...)
  loop_fun <- if (interactive() && requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply
  } else {
    lapply
  }
  simulated <- do.call(loop_fun, loop_args)
  simulated <- do.call(rbind, simulated)
  simulated <- cbind(sample_sizes, simulated)
  colnames(simulated) <- c("size", "mean", "lower", "upper")

  return(simulated)
}
