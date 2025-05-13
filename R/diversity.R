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
is_evenness <- function(x) {
  methods::is(x, "EvennessIndex")
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

# Diversity ====================================================================
#' @export
#' @rdname diversity
#' @aliases diversity,matrix-method
setMethod(
  f = "diversity",
  signature = c(object = "matrix"),
  definition = function(object, ..., evenness = FALSE, unbiased = FALSE) {

    index <- t(apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, evenness, unbiased) {
        c(
          size = sum(x),
          observed = observed(x),
          ## Heterogeneity
          shannon = index_shannon(x, evenness = evenness, unbiased = unbiased),
          brillouin = index_brillouin(x, evenness = evenness),
          ## Dominance
          simpson = index_simpson(x, evenness = evenness, unbiased = unbiased),
          berger = index_berger(x),
          ## Richness
          menhinick = index_menhinick(x),
          margalef = index_margalef(x),
          chao1 = index_chao1(x, unbiased = unbiased),
          ace = index_ace(x),
          squares = index_squares(x)
        )
      },
      evenness = evenness,
      unbiased = unbiased
    ))
    rownames(index) <- rownames(object)
    as.data.frame(index)
  }
)

#' @export
#' @rdname diversity
#' @aliases diversity,data.frame-method
setMethod(
  f = "diversity",
  signature = c(object = "data.frame"),
  definition = function(object, ..., evenness = FALSE, unbiased = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., evenness = evenness, unbiased = unbiased)
  }
)

# Heterogeneity ================================================================
#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,matrix-method
setMethod(
  f = "heterogeneity",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("shannon", "simpson", "berger",
                                   "boone", "brillouin", "mcintosh")) {
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
                        method = c("shannon", "simpson", "berger",
                                   "boone", "brillouin", "mcintosh")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Evenness =====================================================================
#' @export
#' @rdname evenness
#' @aliases evenness,matrix-method
setMethod(
  f = "evenness",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("shannon", "simpson", "brillouin",
                                   "mcintosh")) {
    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method, ..., evenness = TRUE)
    .EvennessIndex(index)
  }
)

#' @export
#' @rdname evenness
#' @aliases evenness,data.frame-method
setMethod(
  f = "evenness",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("shannon", "simpson", "brillouin",
                                   "mcintosh")) {
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
  definition = function(object, n = 1000, f = NULL, level = 0.95,
                        interval = c("basic", "normal", "percentiles"),
                        rare = FALSE) {
    ## Validation
    interval <- match.arg(interval, several.ok = FALSE)

    w <- object@data
    m <- nrow(w)
    method <- object@method
    fun_index <- function(x) {
      do_index(x, method = method, evenness = is_evenness(object))
    }
    if (isTRUE(rare)) {
      fun_resample <- function(x, n) arkhe::resample_uniform(x, n, replace = TRUE)
    } else {
      fun_resample <- function(x, n) arkhe::resample_multinomial(x, n)
    }

    results <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
      hat <- fun_index(w[i, ])
      spl <- t(fun_resample(w[i, ], n = n))
      res <- apply(X = spl, MARGIN = 2, FUN = fun_index)
      if (is.function(f)) {
        results[[i]] <- f(res)
      } else {
        results[[i]] <- summary_bootstrap(res, hat, level = level, interval = interval)
      }
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
#' @method simulate DiversityIndex
simulate.DiversityIndex <- function(object, nsim = 1000, seed = NULL, step = 1,
                                    level = 0.80, interval = "percentiles",
                                    progress = getOption("tabula.progress"), ...) {
  ## Simulate
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    stats::runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  ## Specify the probability for the classes
  data <- object@data
  method <- object@method # Select method

  ## Sample size
  size <- max(rowSums(data))
  sample_sizes <- seq(from = 1, to = size * 1.05, by = step)

  m <- length(sample_sizes)
  k <- seq_len(m)

  results <- vector(mode = "list", length = m)
  fun_index <- function(x) {
    do_index(x, method = method, evenness = is_evenness(object))
  }

  progress_bar <- interactive() && progress
  if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

  for (i in k) {
    spl <- arkhe::resample_multinomial(colSums(data), n = nsim, size = sample_sizes[[i]])
    res <- apply(X = t(spl), MARGIN = 2, FUN = fun_index)
    int <- conf(res, level = level, type = interval)
    results[[i]] <- c(mean = mean(res), int)
    if (progress_bar) utils::setTxtProgressBar(pbar, i)
  }

  if (progress_bar) close(pbar)

  results <- do.call(rbind, results)
  results <- cbind(size = sample_sizes, results)

  methods::initialize(object, simulation = results)
}

#' @export
#' @rdname simulate
#' @aliases simulate,DiversityIndex-method
setMethod("simulate", c(object = "DiversityIndex"), simulate.DiversityIndex)

## Helpers ---------------------------------------------------------------------
summary_bootstrap <- function(x, hat, level = 0.95, interval = "basic") {
  n <- length(x)
  boot_mean <- mean(x)
  boot_bias <- boot_mean - hat
  boot_error <- stats::sd(x)

  ci <- arkhe::confidence_bootstrap(x, level = level, t0 = hat, type = interval)
  results <- c(hat, boot_mean, boot_bias, boot_error, ci)
  names(results) <- c("original", "mean", "bias", "error", "lower", "upper")
  results
}

conf <- function(x, level = 0.80, type = c("percentiles")) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)

  if (type == "percentiles") {
    ## Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(x, probs = c(k, 1 - k), names = FALSE)
  }

  names(conf) <- c("lower", "upper")
  conf
}
