# DEPRECATED METHODS
NULL

#' @rdname deprecate
#' @aliases index_heterogeneity-method
setGeneric(
  name = "index_heterogeneity",
  def = function(object, ...) standardGeneric("index_heterogeneity"),
  valueClass = "HeterogeneityIndex"
)

#' @export
#' @rdname deprecate
#' @aliases index_heterogeneity,ANY-method
setMethod(
  f = "index_heterogeneity",
  signature = signature(object = "ANY"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson")) {
    .Deprecated(new = "heterogeneity", old = "index_heterogeneity")
    heterogeneity(object, method)
  }
)

#' @rdname deprecate
#' @aliases index_evenness-method
setGeneric(
  name = "index_evenness",
  def = function(object, ...) standardGeneric("index_evenness"),
  valueClass = "EvennessIndex"
)

#' @export
#' @rdname deprecate
#' @aliases index_evenness,ANY-method
setMethod(
  f = "index_evenness",
  signature = signature(object = "ANY"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson")) {
    .Deprecated(new = "evenness", old = "index_evenness")
    evenness(object, method)
  }
)

#' @rdname deprecate
#' @aliases index_richness-method
setGeneric(
  name = "index_richness",
  def = function(object, ...) standardGeneric("index_richness"),
  valueClass = "RichnessIndex"
)

#' @export
#' @rdname deprecate
#' @aliases index_richness,ANY-method
setMethod(
  f = "index_richness",
  signature = signature(object = "ANY"),
  definition = function(object, method = c("count", "margalef", "menhinick")) {
    .Deprecated(new = "richness", old = "index_richness")
    richness(object, method)
  }
)

#' @rdname deprecate
#' @aliases index_composition-method
setGeneric(
  name = "index_composition",
  def = function(object, ...) standardGeneric("index_composition"),
  valueClass = "CompositionIndex"
)

#' @export
#' @rdname deprecate
#' @aliases index_composition,ANY-method
setMethod(
  f = "index_composition",
  signature = signature(object = "ANY"),
  definition = function(object, method = c("chao1", "ace", "chao2", "ice"),
                        unbiased = FALSE, improved = FALSE, k = 10) {
    .Deprecated(new = "composition", old = "index_composition")
    composition(object, method, unbiased = unbiased, improved = improved, k = k)
  }
)

#' @rdname deprecate
#' @aliases simulate_heterogeneity-method
setGeneric(
  name = "simulate_heterogeneity",
  def = function(object, ...) standardGeneric("simulate_heterogeneity"),
  valueClass = "HeterogeneityIndex"
)

#' @rdname deprecate
#' @aliases bootstrap_heterogeneity-method
setGeneric(
  name = "bootstrap_heterogeneity",
  def = function(object, ...) standardGeneric("bootstrap_heterogeneity"),
  valueClass = "data.frame"
)

#' @rdname deprecate
#' @aliases jackknife_heterogeneity-method
setGeneric(
  name = "jackknife_heterogeneity",
  def = function(object, ...) standardGeneric("jackknife_heterogeneity"),
  valueClass = "data.frame"
)

#' @rdname deprecate
#' @aliases simulate_evenness-method
setGeneric(
  name = "simulate_evenness",
  def = function(object, ...) standardGeneric("simulate_evenness"),
  valueClass = "EvennessIndex"
)

#' @rdname deprecate
#' @aliases bootstrap_evenness-method
setGeneric(
  name = "bootstrap_evenness",
  def = function(object, ...) standardGeneric("bootstrap_evenness"),
  valueClass = "data.frame"
)

#' @rdname deprecate
#' @aliases jackknife_evenness-method
setGeneric(
  name = "jackknife_evenness",
  def = function(object, ...) standardGeneric("jackknife_evenness"),
  valueClass = "data.frame"
)

#' @rdname deprecate
#' @aliases simulate_richness-method
setGeneric(
  name = "simulate_richness",
  def = function(object, ...) standardGeneric("simulate_richness"),
  valueClass = "RichnessIndex"
)

#' @rdname deprecate
#' @aliases bootstrap_richness-method
setGeneric(
  name = "bootstrap_richness",
  def = function(object, ...) standardGeneric("bootstrap_richness"),
  valueClass = "data.frame"
)

#' @rdname deprecate
#' @aliases jackknife_richness-method
setGeneric(
  name = "jackknife_richness",
  def = function(object, ...) standardGeneric("jackknife_richness"),
  valueClass = "data.frame"
)

#' @export
#' @rdname deprecate
#' @aliases bootstrap_heterogeneity,CountMatrix-method
setMethod(
  f = "bootstrap_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        probs = c(0.05, 0.95), n = 1000, ...) {
    .Deprecated(new = "bootstrap", old = "bootstrap_heterogeneity")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    bootstrap_diversity(object, method = fun, probs = probs, n = n)
  }
)

#' @export
#' @rdname deprecate
#' @aliases jackknife_heterogeneity,CountMatrix-method
setMethod(
  f = "jackknife_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"), ...) {
    .Deprecated(new = "jackknife", old = "jackknife_heterogeneity")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    jackknife_diversity(object, method = fun)
  }
)

#' @export
#' @rdname deprecate
#' @aliases simulate_heterogeneity,CountMatrix-method
setMethod(
  f = "simulate_heterogeneity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        quantiles = TRUE, level = 0.80, step = 1, n = 1000,
                        progress = getOption("tabula.progress"), ...) {
    .Deprecated(new = "simulate", old = "simulate_heterogeneity")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    index <- simulate_diversity(
      object,
      method = fun,
      quantiles = quantiles,
      level = level,
      step = step,
      n = n,
      progress = progress
    )
    .HeterogeneityIndex(index, method = method)
  }
)

#' @export
#' @rdname deprecate
#' @aliases bootstrap_evenness,CountMatrix-method
setMethod(
  f = "bootstrap_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        probs = c(0.05, 0.95), n = 1000, ...) {
    .Deprecated(new = "bootstrap", old = "bootstrap_evenness")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    bootstrap_diversity(object, method = fun, probs = probs, n = n,
                        evenness = TRUE)
  }
)

#' @export
#' @rdname deprecate
#' @aliases jackknife_evenness,CountMatrix-method
setMethod(
  f = "jackknife_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"), ...) {
    .Deprecated(new = "jackknife", old = "jackknife_evenness")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    jackknife_diversity(object, method = fun, evenness = TRUE)
  }
)

#' @export
#' @rdname deprecate
#' @aliases simulate_evenness,CountMatrix-method
setMethod(
  f = "simulate_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson"),
                        quantiles = TRUE, level = 0.80, step = 1, n = 1000,
                        progress = getOption("tabula.progress"), ...) {
    .Deprecated(new = "simulate", old = "simulate_evenness")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    index <- simulate_diversity(
      object,
      method = fun,
      quantiles = quantiles,
      level = level,
      step = step,
      n = n,
      progress = progress,
      evenness = TRUE
    )
    .EvennessIndex(index, method = method)
  }
)

#' @export
#' @rdname deprecate
#' @aliases bootstrap_richness,CountMatrix-method
setMethod(
  f = "bootstrap_richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("none", "margalef", "menhinick"),
                        probs = c(0.05, 0.95), n = 1000, ...) {
    .Deprecated(new = "bootstrap", old = "bootstrap_richness")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    bootstrap_diversity(object, method = fun, probs = probs, n = n)
  }
)

#' @export
#' @rdname deprecate
#' @aliases jackknife_richness,CountMatrix-method
setMethod(
  f = "jackknife_richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("none", "margalef", "menhinick"),
                        ...) {
    .Deprecated(new = "jackknife", old = "jackknife_richness")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    jackknife_diversity(object, method = fun)
  }
)

#' @export
#' @rdname deprecate
#' @aliases simulate_richness,CountMatrix-method
setMethod(
  f = "simulate_richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("none", "margalef", "menhinick"),
                        quantiles = TRUE, level = 0.80, step = 1, n = 1000,
                        progress = getOption("tabula.progress"), ...) {
    .Deprecated(new = "simulate", old = "simulate_richness")
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method) # Select method
    index <- simulate_diversity(
      object,
      method = fun,
      quantiles = quantiles,
      level = level,
      step = step,
      n = n,
      progress = progress
    )
    .RichnessIndex(index, method = method)
  }
)

#' Bootstrap a Diversity Index
#'
#' @param object A [`numeric`] [`matrix`].
#' @param method A [`function`].
#' @param probs A [`numeric`] vector of probabilities with values in
#'  \eqn{[0,1]} (see [stats::quantile()]).
#' @param n A non-negative [`integer`] giving the number of bootstrap
#' replications.
#' @param ... Further parameters to be passed to `method`.
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
bootstrap_diversity <- function(object, method, probs = c(0.05, 0.95),
                                n = 1000, ...) {
  results <- apply(
    X = object,
    MARGIN = 1,
    FUN = stats_bootstrap,
    do = method,
    probs = probs,
    n = n,
    na.rm = TRUE,
    ...
  )
  as.data.frame(t(results))
}

#' Bootstrap Estimation
#'
#' @param x A vector.
#' @param do A [`function`] that takes `x` as an argument
#'  and returns a single numeric value.
#' @param probs A [`numeric`] vector of probabilities with values in
#'  \eqn{[0,1]} (see [stats::quantile()]).
#' @param n A non-negative [`integer`] giving the number of bootstrap
#' replications.
#' @param na.rm A [`logical`] scalar: should missing values be removed
#' from `x` before the quantiles are computed?
#' @param ... Extra arguments passed to `do`.
#' @return A [`numeric`] vector with the following elements:
#'  \describe{
#'   \item{`min`}{Minimum value.}
#'   \item{`mean`}{Mean value.}
#'   \item{`max`}{Maximum value.}
#'   \item{`Q*`}{Sample quantile to * probability.}
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

#' Jackknife a Diversity Index
#'
#' @param object A [`numeric`] [`matrix`].
#' @param method A [`function`].
#' @param ... Further parameters to be passed to `method`.
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
jackknife_diversity <- function(object, method, ...) {
  results <- apply(
    X = object,
    MARGIN = 1,
    FUN = stats_jackknife,
    do = method,
    ...
  )
  as.data.frame(t(results))
}

#' Jackknife Estimation
#'
#' @param x A vector.
#' @param do A [`function`] that takes `x` as an argument
#'  and returns a single numeric value.
#' @param ... Extra arguments passed to `do`.
#' @return A [`numeric`] vector with the following elements:
#'  \describe{
#'   \item{`values`}{The \eqn{n} leave-one-out values.}
#'   \item{`mean`}{The jackknife estimate of mean.}
#'   \item{`bias`}{The jackknife estimate of bias.}
#'   \item{`error`}{he jackknife estimate of standard error.}
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

#' Simulate a Diversity Index
#'
#' @param object A [`numeric`] [`matrix`].
#' @param method A [`function`].
#' @param quantiles A [`logical`] scalar: should sample quantiles
#'  be used as confidence interval? If `TRUE` (the default),
#'  sample quantiles are used as described in Kintigh (1989), else quantiles of
#'  the normal distribution are used.
#' @param level A length-one [`numeric`] vector giving the
#'  confidence level.
#' @param step An [`integer`].
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param progress A [`logical`] scalar: should a progress bar be
#'  displayed?
#' @param ... Further parameters to be passed to `method`.
#' @keywords internal
#' @noRd
simulate_diversity <- function(object, method, quantiles = TRUE,
                               level = 0.80, step = 1, n = 1000,
                               progress = getOption("tabula.progress"), ...) {

  ## Index
  index <- index_diversity(object, method)

  ## Simulate
  ## Specify the probability for the classes
  prob <- colSums(object) / sum(object)

  ## Sample size
  size <- max(rowSums(object))
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
      quantiles = quantiles,
      ...
    )
    if (progress_bar) utils::setTxtProgressBar(pbar, i)
  }

  if (progress_bar) close(pbar)

  simulated <- do.call(rbind, simulated)
  simulated <- cbind(`size` = sample_sizes, simulated)

  methods::initialize(index, simulation = simulated)
}

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
