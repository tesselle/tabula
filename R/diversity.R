# DIVERSITY
#' @include AllGenerics.R
NULL

# Index ========================================================================
index_observed <- function(x, ...) {
  sum(x > 0, ...)  # Number of observed species
}
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

# Heterogeneity ================================================================
#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,matrix-method
setMethod(
  f = "heterogeneity",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("berger", "boone", "brillouin",
                                   "mcintosh", "shannon", "simpson")) {
    method <- match.arg(method, several.ok = FALSE)
    by_row <- method != "boone"
    index <- index_diversity(object, method, ..., evenness = FALSE,
                             by_row = by_row)
    .HeterogeneityIndex(index)
  }
)

#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,data.frame-method
setMethod(
  f = "heterogeneity",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("berger", "boone", "brillouin",
                                   "mcintosh", "shannon", "simpson")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Evenness =====================================================================
#' @export
#' @rdname heterogeneity
#' @aliases evenness,matrix-method
setMethod(
  f = "evenness",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("shannon", "brillouin",
                                   "mcintosh", "simpson")) {
    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method, ..., evenness = TRUE)
    .EvennessIndex(index)
  }
)

#' @export
#' @rdname heterogeneity
#' @aliases evenness,data.frame-method
setMethod(
  f = "evenness",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("shannon", "brillouin",
                                   "mcintosh", "simpson")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Richness =====================================================================
#' @export
#' @rdname richness
#' @aliases richness,matrix-method
setMethod(
  f = "richness",
  signature = c(object = "matrix"),
  definition = function(object, ..., method = c("observed", "margalef", "menhinick")) {
    ## Backward compatibility
    if (method == "count") method <- "observed"

    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method, ...)
    .RichnessIndex(index)
  }
)

#' @export
#' @rdname richness
#' @aliases richness,data.frame-method
setMethod(
  f = "richness",
  signature = c(object = "data.frame"),
  definition = function(object, ..., method = c("observed", "margalef", "menhinick")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Composition ==================================================================
#' @export
#' @rdname richness
#' @aliases composition,matrix-method
setMethod(
  f = "composition",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("chao1", "ace", "squares", "chao2", "ice")) {
    method <- match.arg(method, several.ok = FALSE)
    by_row <- any(method == c("chao1", "ace", "squares"))
    index <- index_diversity(object, method, ..., by_row = by_row)
    .CompositionIndex(index)
  }
)

#' @export
#' @rdname richness
#' @aliases composition,data.frame-method
setMethod(
  f = "composition",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("chao1", "ace", "squares", "chao2", "ice")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Turnover =====================================================================
#' @export
#' @rdname turnover
#' @aliases turnover,matrix-method
setMethod(
  f = "turnover",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("whittaker", "cody", "routledge1",
                                   "routledge2", "routledge3", "wilson")) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method)
    fun(object)
  }
)

#' @export
#' @rdname turnover
#' @aliases turnover,data.frame-method
setMethod(
  f = "turnover",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("whittaker", "cody", "routledge1",
                                   "routledge2", "routledge3", "wilson")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Resample =====================================================================
## Bootstrap -------------------------------------------------------------------
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,DiversityIndex-method
setMethod(
  f = "bootstrap",
  signature = c(object = "DiversityIndex"),
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

## Jackknife -------------------------------------------------------------------
#' @export
#' @rdname jackknife
#' @aliases jackknife,DiversityIndex-method
setMethod(
  f = "jackknife",
  signature = c(object = "DiversityIndex"),
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

## Simulate --------------------------------------------------------------------
#' @export
#' @rdname simulate
#' @aliases simulate,DiversityIndex-method
setMethod(
  f = "simulate",
  signature = c(object = "DiversityIndex"),
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
