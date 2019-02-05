#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname date
#' @aliases dateEvent,CountMatrix-method
setMethod(
  f = "dateEvent",
  signature = signature(object = "CountMatrix"),
  definition = function(object, dates, axes = 5,
                        level = 0.95, n = 500, verbose = FALSE, ...) {
    # Validation
    assemblages <- rownames(object)
    fabrics <- colnames(object)
    known_dates <- names(dates)
    active_dates <- which(assemblages %in% known_dates)

    matched <- length(dates) - length(active_dates)
    if (matched != 0) {
      stop(paste(matched, "dated assemblage(s) do not match", sep = " "))
    }

    # Correspondance analysis
    corresp <- FactoMineR::CA(object, ..., ncp = axes, graph = FALSE)
    row_coord <- as.data.frame(corresp$row$coord)
    col_coord <- as.data.frame(corresp$col$coord)
    if (verbose) { FactoMineR::summary.CA(corresp) }

    # Event date
    ## Gaussian multiple linear regression model
    contexts <- cbind.data.frame(dates = dates, row_coord[active_dates, ])
    fit <- stats::lm(dates ~ ., data = contexts)
    fit_summary <- stats::summary.lm(fit)
    if (verbose) { print(fit_summary) }

    ## Predict event date for each context
    row_predict <- stats::predict.lm(fit, row_coord, se.fit = TRUE,
                                     interval = "confidence", level = level)
    row_fit <- cbind(row_predict$fit, error = row_predict$se.fit)
    colnames(row_fit) <- c("estimation", "earliest", "latest", "error")

    ## Check predicted dates
    mapply(FUN = function(name, date, start, stop, level) {
      if (date < start | date > stop) {
        warning(paste("Model fitting: context ", name,
                      " does not lie in the predicted ", level * 100,
                      "% confidence interval.", sep = ""), call. = FALSE)
      }
    },
    name = known_dates, date = dates,
    start = row_fit[active_dates , "earliest", drop = FALSE],
    stop = row_fit[active_dates, "latest", drop = FALSE],
    MoreArgs = list(level))

    ## Predict event dates for each fabric
    col_predict <- stats::predict.lm(fit, col_coord, se.fit = TRUE,
                                     interval = "confidence", level = level)
    col_fit <- cbind(col_predict$fit, error = col_predict$se.fit)
    colnames(col_fit) <- c("estimation", "earliest", "latest", "error")

    # col_fit <- apply(X = row_fit, MARGIN = 2, FUN = function(x, object) {
    #   # CA transition formulae
    #   diag(1 / colSums(object)) %*% t(object) %*% matrix(x, ncol = 1)
    # }, object)
    # rownames(col_fit) <- colnames(object)

    # Accumulation time
    col_dates <- col_fit[, "estimation"]
    col_errors <- col_fit[, "error"]
    date_range <- seq(from = min(row_fit[, "earliest"]),
                      to = max(row_fit[, "latest"]), length.out = n)

    ## Weighted sum of the fabric dates
    # weighted_mean <- apply(
    #   X = counts, MARGIN = 1,
    #   FUN = function(weights, dates) {
    #     stats::weighted.mean(x = dates, w = weights)
    #   }, dates = col_dates
    # )

    ## Gaussian mixture
    freq <- object / rowSums(object)
    col_density <- mapply(function(mean, sd, x) { stats::dnorm(x, mean, sd) },
                          mean = col_dates, sd = col_errors,
                          MoreArgs = list(x = date_range), SIMPLIFY = TRUE)

    acc_density <- apply(X = freq, MARGIN = 1, FUN = function(x, density) {
      colSums(t(density) * as.numeric(x))
    }, density = col_density)

    ## Point estimate of accumulation time (median)
    acc_median <- apply(X = acc_density, MARGIN = 2, FUN = function(x, range) {
      cumulative_sum <- cumsum(x)
      index <- cumulative_sum / max(cumulative_sum)
      range[which.max(index >= 0.5)]
    }, range = date_range)

    methods::new("DateModel",
                 model = fit,
                 level = level,
                 residual = row_predict$residual.scale,
                 rows = row_fit,
                 columns = col_fit,
                 accumulation = acc_median)
  }
)
