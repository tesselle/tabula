#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname richness
#' @aliases simulate_richness,CountMatrix-method
setMethod(
  f = "simulate_richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = 0.80, n = 1000) {

    fun <- function(x) { sum(x > 0) }
    replicates <- simulate_diversity(x = object, method = fun,
                                     prob = NULL, level = level, n = n)
    return(replicates)
  }
)

#' @export
#' @rdname richness
#' @aliases simulate_richness,numeric-method
setMethod(
  f = "simulate_richness",
  signature = signature(object = "numeric"),
  definition = function(object, size, level = 0.80, n = 1000) {

    fun <- function(x) { sum(x > 0) }
    replicates <- simulate_diversity(x = size, method = fun,
                                     prob = object, level = level, n = n)
    return(replicates)
  }
)

#' @export
#' @rdname diversity
#' @aliases simulate_evenness,CountMatrix-method
setMethod(
  f = "simulate_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, prob = NULL, level = 0.80, n = 1000) {

    fun <- function(x) { evennessShannon(x, zero.rm = FALSE) }
    replicates <- simulate_diversity(x = object, method = fun,
                                     prob = prob, level = level, n = n)
    return(replicates)
  }
)

#' @export
#' @rdname diversity
#' @aliases simulate_evenness,numeric-method
setMethod(
  f = "simulate_evenness",
  signature = signature(object = "numeric"),
  definition = function(object, size, level = 0.80, n = 1000) {

    fun <- function(x) { evennessShannon(x, zero.rm = FALSE) }
    replicates <- simulate_diversity(x = size, method = fun,
                                     prob = object, level = level, n = n)
    return(replicates)
  }
)

simulate_diversity <- function(x, method, prob = NULL, level = 0.80, n = 1000) {
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

  sim <- function(x, n, prob, level, method) {
    replicates <- stats::rmultinom(n, size = x, prob = prob)
    R <- apply(X = replicates, MARGIN = 2, FUN = method)

    # Confidence interval as described in Kintigh 1989
    # k <- (1 - level) / 2
    # conf <- quantile(R, probs = c(k, k + level))

    # Confidence interval
    conf <- mean(R) + stats::sd(R) * stats::qnorm(level / 2) * c(-1, 1)

    c(mean(R), conf)
  }
  loop_args <- list(X = sample_sizes, FUN = sim,
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
