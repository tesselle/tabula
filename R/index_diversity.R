# DIVERSITY INDEX
#' @include AllGenerics.R AllClasses.R
NULL

# Index ========================================================================
get_index <- function(x) {
  match.fun(sprintf("index_%s", x))
}
boot_index <- function(x, i, method, ...) {
  x <- x[i]
  f <- get_index(method)
  f(x, ...)
}

#' Compute a Diversity Index
#'
#' @param x A [`numeric`] [`matrix`].
#' @param method A [`character`] string specifying the measure to be computed.
#' @param by_row A [`logical`] scalar: should `method` be computed for each row?
#' @param ... Further parameters to be passed to `method`.
#' @return A [DiversityIndex-class] object.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
index_diversity <- function(x, method, ..., by_row = TRUE) {
  fun <- get_index(method)
  if (by_row) {
    idx <- apply(X = x, MARGIN = 1, FUN = fun, ...)
  } else {
    idx <- fun(x, ...)
  }

  ## Fix names
  n <- rownames(x)
  if (is.null(n)) n <- paste0("S", seq_along(idx))

  .DiversityIndex(
    idx,
    names = n,
    size = as.integer(rowSums(x)),
    data = x,
    method = method
  )
}

# Resample =====================================================================
#' @export
#' @rdname resample
#' @aliases bootstrap,DiversityIndex-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "DiversityIndex"),
  definition = function(object, n = 1000, f = NULL) {

    w <- object@data
    m <- nrow(w)
    method <- object@method
    evenness <- methods::is(object, "EvennessIndex")

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      b <- boot::boot(w[i, ], statistic = boot_index, R = n, method = method,
                      evenness = evenness)
      if (is.null(f)) {
        results[[i]] <- summary_bootstrap(b$t, b$t0)
      } else {
        results[[i]] <- f(as.numeric(b$t))
      }
    }
    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    as.data.frame(results)
  }
)

summary_bootstrap <- function(x, hat) {
  n <- length(x)
  boot_mean <- mean(x)
  boot_bias <- boot_mean - hat
  boot_error <- stats::sd(x)

  results <- c(hat, boot_mean, boot_bias, boot_error)
  names(results) <- c("original", "mean", "bias", "error")
  results
}

#' @export
#' @rdname resample
#' @aliases jackknife,DiversityIndex-method
setMethod(
  f = "jackknife",
  signature = signature(object = "DiversityIndex"),
  definition = function(object) {

    w <- object@data
    m <- nrow(w)

    fun <- get_index(object@method) # Select method
    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      results[[i]] <- arkhe::jackknife(
        object = w[i, ],
        do = fun,
        evenness = methods::is(object, "EvennessIndex")
      )
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
  definition = function(object, n = 1000, step = 1,
                        interval = c("percentiles", "student", "normal"),
                        level = 0.80, progress = getOption("tabula.progress")) {
    ## Simulate
    ## Specify the probability for the classes
    data <- object@data
    method <- get_index(object@method) # Select method

    ## Sample size
    size <- max(rowSums(data))
    sample_sizes <- seq(from = 1, to = size * 1.05, by = step)

    m <- length(sample_sizes)
    k <- seq_len(m)

    simulated <- vector(mode = "list", length = m)
    fun <- function(x) conf(x, level = level, type = interval)

    progress_bar <- interactive() && progress
    if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

    for (i in k) {
      simulated[[i]] <- resample(
        object = colSums(data),
        do = method,
        evenness = methods::is(object, "EvennessIndex"),
        n = n,
        size = sample_sizes[[i]],
        f = fun
      )
      if (progress_bar) utils::setTxtProgressBar(pbar, i)
    }

    if (progress_bar) close(pbar)

    simulated <- do.call(rbind, simulated)
    simulated <- cbind(size = sample_sizes, simulated)

    methods::initialize(object, simulation = simulated)
  }
)

conf <- function(x, type = c("percentiles", "student", "normal"),
                 level = 0.80) {
  type <- match.arg(type, several.ok = FALSE)
  if (type == "percentiles") {
    ## Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(x, probs = c(k, 1 - k), names = FALSE)
  } else {
    ## Confidence interval
    conf <- arkhe::confidence(x, level = level, type = type)
  }

  result <- c(mean(x), conf)
  names(result) <- c("mean", "lower", "upper")
  result
}
