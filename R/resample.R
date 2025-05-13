# RESAMPLE
#' @include AllGenerics.R
NULL

# Bootstrap ====================================================================
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,DiversityIndex-method
setMethod(
  f = "bootstrap",
  signature = c(object = "DiversityIndex"),
  definition = function(object, n = 1000, f = NULL, level = 0.95,
                        interval = c("basic", "normal", "percentiles"),
                        seed = NULL, rare = FALSE) {
    ## Validation
    interval <- match.arg(interval, several.ok = FALSE)

    ## Seed
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      stats::runif(1)
    if (is.null(seed)) {
      RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
      R.seed <- get(".Random.seed", envir = .GlobalEnv)
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }

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
    results <- as.data.frame(results)

    attr(results, "seed") <- RNGstate
    results
  }
)

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

# Jackknife ====================================================================
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

# Simulate =====================================================================
#' @export
#' @method simulate DiversityIndex
simulate.DiversityIndex <- function(object, nsim = 1000, seed = NULL, step = 1,
                                    level = 0.80, interval = "percentiles",
                                    progress = getOption("tabula.progress"), ...) {
  ## Validation
  interval <- match.arg(interval, several.ok = FALSE)

  ## Seed
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    stats::runif(1)
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  ## Specify the probability for the classes
  data <- object@data
  ## Select method
  method <- object@method

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

  methods::initialize(object, simulation = results, seed = RNGstate)
}

#' @export
#' @rdname simulate
#' @aliases simulate,DiversityIndex-method
setMethod("simulate", c(object = "DiversityIndex"), simulate.DiversityIndex)

conf <- function(x, level = 0.80, type = c("percentiles")) {
  if (type == "percentiles") {
    ## Confidence interval as described in Kintigh 1989
    k <- (1 - level) / 2
    conf <- stats::quantile(x, probs = c(k, 1 - k), names = FALSE)
  }

  names(conf) <- c("lower", "upper")
  conf
}
