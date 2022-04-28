# DIVERSITY INDEX
#' @include AllGenerics.R AllClasses.R
NULL

# Index ========================================================================
#' Compute a Diversity Index
#'
#' @param x A [`numeric`] [`matrix`].
#' @param method A [`function`].
#' @param ... Further parameters to be passed to `method`.
#' @return A [DiversityIndex-class] object.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
index_diversity <- function(x, method, ...) {
  idx <- apply(X = x, MARGIN = 1, FUN = method, ...)
  .DiversityIndex(
    names = rownames(x),
    values = idx,
    size = as.integer(rowSums(x)),
    data = x
  )
}

# Resample =====================================================================
#' @export
#' @rdname resample
#' @aliases bootstrap,DiversityIndex-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "DiversityIndex"),
  definition = function(object, level = 0.95, type = c("student", "normal"),
                        probs = c(0.25, 0.50, 0.75), n = 1000) {

    theta <- function(x, do, n, level, type, probs) {
      boot <- dimensio::bootstrap(x, do, n)
      dimensio::summary(boot, level = level, type = type, probs = probs,
                        na.rm = TRUE)
    }
    results <- apply(
      X = object@data,
      MARGIN = 1,
      FUN = theta,
      do = get_index(object), # Select method
      n = n,
      level = level,
      type = type,
      probs = probs
    )
    as.data.frame(t(results))
  }
)

#' @export
#' @rdname resample
#' @aliases jackknife,DiversityIndex-method
setMethod(
  f = "jackknife",
  signature = signature(object = "DiversityIndex"),
  definition = function(object) {

    w <- object@data
    m <- nrow(w)

    fun <- get_index(object) # Select method
    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      jack <- dimensio::jackknife(
        object = w[i, ],
        do = fun
      )
      results[[i]] <- dimensio::summary(jack)
    }
    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    as.data.frame(results)
  }
)

# Simulate =====================================================================
#' @export
#' @rdname simulate
#' @aliases simulate,DiversityIndex-method
setMethod(
  f = "simulate",
  signature = signature(object = "DiversityIndex"),
  definition = function(object, quantiles = TRUE, level = 0.80, step = 1,
                        n = 1000, progress = getOption("tabula.progress")) {
    ## Simulate
    ## Specify the probability for the classes
    data <- object@data
    method <- get_index(object) # Select method
    prob <- colSums(data) / sum(data)

    ## Sample size
    size <- max(rowSums(data))
    sample_sizes <- seq(from = 1, to = size * 1.05, by = step)

    m <- length(sample_sizes)
    k <- seq_len(m)

    simulated <- vector(mode = "list", length = m)

    progress_bar <- interactive() && progress
    if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

    for (i in k) {
      simulated[[i]] <- sim(
        size = sample_sizes[[i]],
        prob = prob,
        method = method,
        n = n,
        level = level,
        quantiles = quantiles
      )
      if (progress_bar) utils::setTxtProgressBar(pbar, i)
    }

    if (progress_bar) close(pbar)

    simulated <- do.call(rbind, simulated)
    simulated <- cbind(size = sample_sizes, simulated)

    methods::initialize(object, simulation = simulated)
  }
)

#' @param size A [`numeric`] matrix.
#' @param prob A length-\eqn{p} [`numeric`] vector giving the of
#'  probability of the \eqn{p} taxa/types (see below). If `NULL` (the
#'  default), probabilities are estimated from `x`.
#' @param method A [`function`].
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param level A length-one [`numeric`] vector giving the
#'  confidence level.
#' @param quantiles A [`logical`] scalar: should sample quantiles
#'  be used as confidence interval? If `TRUE` (the default),
#'  sample quantiles are used as described in Kintigh (1989), else quantiles of
#'  the normal distribution are used.
#' @param na.rm A [`logical`] scalar: should missing values be removed
#'  before the quantiles are computed?
#' @param ... Further parameters to be passed to `method`.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
sim <- function(size, prob, method, n = 1000, level = 0.80,
                quantiles = TRUE, na.rm = TRUE, ...) {

  replicates <- stats::rmultinom(n = n, size = size, prob = prob)
  R <- apply(X = replicates, MARGIN = 2, FUN = method, ...)

  if (quantiles) {
    ## Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(R, probs = c(k, 1 - k), na.rm = na.rm, names = FALSE)
  } else {
    ## Confidence interval
    conf <- mean(R) + stats::sd(R) * stats::qnorm(k) * c(-1, 1)
  }

  result <- c(mean(R), conf)
  names(result) <- c("mean", "lower", "upper")
  return(result)
}
