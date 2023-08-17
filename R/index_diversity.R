# DIVERSITY INDEX
#' @include AllGenerics.R AllClasses.R
NULL

# Index ========================================================================
get_index <- function(x) {
  match.fun(sprintf("index_%s", x))
}
do_index <- function(x, method, ...) {
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
  row_names <- rownames(x) %||% paste0("S", seq_along(idx))

  .DiversityIndex(
    idx,
    labels = row_names,
    size = as.integer(rowSums(x)),
    data = x,
    method = method
  )
}

# Resample =====================================================================
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,DiversityIndex-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "DiversityIndex"),
  definition = function(object, n = 1000, f = NULL) {

    w <- object@data
    m <- nrow(w)
    method <- object@method

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      results[[i]] <- arkhe::bootstrap(
        object = w[i, ],
        do = do_index,
        n = n,
        method = method,
        evenness = methods::is(object, "EvennessIndex"),
        f = f
      )
    }
    results <- do.call(rbind, results)
    rownames(results) <- rownames(w)
    as.data.frame(results)
  }
)

#' @export
#' @rdname jackknife
#' @aliases jackknife,DiversityIndex-method
setMethod(
  f = "jackknife",
  signature = signature(object = "DiversityIndex"),
  definition = function(object, f = NULL) {

    w <- object@data
    m <- nrow(w)
    method <- object@method

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      results[[i]] <- arkhe::jackknife(
        object = w[i, ],
        do = do_index,
        method = method,
        evenness = methods::is(object, "EvennessIndex"),
        f = f
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
    method <- object@method # Select method

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
        do = do_index,
        method = method,
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
    conf <- arkhe::confidence_mean(x, level = level, type = type)
  }

  result <- c(mean(x), conf)
  names(result) <- c("mean", "lower", "upper")
  result
}
