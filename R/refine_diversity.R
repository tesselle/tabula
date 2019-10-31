#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname richness
#' @aliases refine_richness,CountMatrix-method
setMethod(
  f = "refine_richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, prob = NULL, level = 0.80, n = 1000) {

    fun <- function(x) { sum(x > 0) }
    replicates <- refine_diversity(x = object, method = fun,
                                   prob = prob, level = level, n = n)
    return(replicates)
  }
)

#' @export
#' @rdname diversity
#' @aliases refine_evenness,CountMatrix-method
setMethod(
  f = "refine_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, prob = NULL, level = 0.80, n = 1000) {

    fun <- function(x) { evennessShannon(x, zero.rm = FALSE) }
    replicates <- refine_diversity(x = object, method = fun,
                                   prob = prob, level = level, n = n)
    return(replicates)
  }
)

refine_diversity <- function(x, method, prob = NULL, level = 0.80, n = 1000) {
  # Specify the probability for the classes
  if (is.null(prob)) {
    prob <- colSums(x) / sum(x)
  }
  # Sample size
  size <- max(rowSums(x))
  sample_sizes <- seq_len(size * 1.05)


  simulate_diversity <- function(x, n, prob, level, method) {
    replicates <- stats::rmultinom(n, size = x, prob = prob)
    R <- apply(X = replicates, MARGIN = 2, FUN = method)

    # Confidence interval as described in Kintigh 1989
    # k <- (1 - level) / 2
    # conf <- quantile(R, probs = c(k, k + level))

    # Confidence interval
    conf <- mean(R) + stats::sd(R) * stats::qnorm(level / 2) * c(-1, 1)

    c(mean(R), conf)
  }
  loop_args <- list(X = sample_sizes, FUN = simulate_diversity,
                    n = n, prob = prob, level = level, method = method)
  loop_fun <- if (requireNamespace("pbapply", quietly = TRUE)) {
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
