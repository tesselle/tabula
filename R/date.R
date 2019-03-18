# DATE MODEL
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname date
#' @aliases dateEvent,CountMatrix-method
setMethod(
  f = "dateEvent",
  signature = signature(object = "CountMatrix"),
  definition = function(object, dates, level = 0.95, cutoff = 90,
                        jackknife = FALSE, bootstrap = FALSE, n = 1000, ...) {
    # Validation
    if (!is.list(dates))
      stop("A list of dates is expected.")
    if (cutoff < 50)
      stop("Cutoff value is below 50%, you can't be serious.")

    ## Coerce dates to numeric values and convert to a two columns data frame
    dates <- lapply(X = dates,
                    FUN = function(x) data.frame(date = as.numeric(x))) %>%
      dplyr::bind_rows(., .id = "id")
    ## Get count data
    counts <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    ## Match date and counts names
    active_dates <- sum(rownames(counts) %in% dates$id)
    unmatched <- nrow(dates) - active_dates
    if (unmatched != 0) {
      stop(paste(unmatched, "date(s) do not match with contexts.", sep = " "))
    }

    # Correspondance analysis
    axes <- min(dim(counts))
    results_CA <- FactoMineR::CA(counts, ncp = axes, graph = FALSE, ...)
    keep_dim <- which(results_CA$eig[, 3] <= cutoff)
    row_coord <- results_CA$row$coord[, keep_dim]
    col_coord <- results_CA$col$coord[, keep_dim]

    # Event date
    ## Gaussian multiple linear regression model
    contexts <- merge(dates, row_coord, by.x = "id", by.y = "row.names")
    contexts %<>% as.data.frame() %>% dplyr::select(-1)
    fit <- stats::lm(date ~ ., data = contexts)

    ## Predict event date for each context
    row_event <- predictEvent(fit, row_coord, level = level)
    ## Predict event dates for each fabric
    col_event <- predictEvent(fit, col_coord, level = level)
    # Pour mémoire:
    # col_event <- apply(X = row_event$fit, MARGIN = 2, FUN = function(x, counts) {
    #   # CA transition formulae
    #   diag(1 / colSums(counts)) %*% t(counts) %*% matrix(x, ncol = 1)
    # }, counts)
    # rownames(col_event) <- colnames(counts)

    # TODO: check predicted dates consistency

    # Accumulation time point estimate
    date_range <- range(row_event$fit[, c("earliest", "latest")])
    acc_estimate <- predictAccumulation(x = counts, event = col_event,
                                        range = date_range)

    # Check model with resampling methods
    jack_event <- boot_event <- data.frame()
    ## Jackknife fabrics
    if (jackknife) {
      jack_event <- jackDate(counts, dates, fit, keep = keep_dim,
                             level = level, ...)
      # Compute jaccknife bias
      jack_event %<>% dplyr::mutate(
        bias = (ncol(counts) - 1) *
          (.data$estimation - row_event$fit$estimation)
      )
    }
    ## Bootstrap assemblages
    if (bootstrap) {
      boot_event <- bootDate(counts, fit, margin = 1, n = n,
                             keep = keep_dim, level = level, ...)
    }

    methods::new(
      "DateModel",
      counts = counts,
      dates = dates,
      level = level,
      model = fit,
      residual = row_event$residual,
      rows = row_event$fit,
      columns = col_event$fit,
      accumulation = acc_estimate,
      jackknife = jack_event,
      bootstrap = boot_event
    )
  }
)

# Internal functions ===========================================================
#' Predict event dates
#'
#' @param fit A \code{\link[stats:lm]{multiple linear model}}.
#' @param coord A \code{\link{numeric}} matrix giving the coordinates in CA
#'  space.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @return
#'  A list with the following components:
#'  \describe{
#'   \item{fit}{A five columns \code{\link{data.frame}} giving the predicted
#'   event dates, the corresponding confidence interval boundaries, standard
#'   error of the predicted dates and an identifier.}
#'   \item{residual}{Residual standard deviations.}
#'  }
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
predictEvent <- function(fit, coord, level) {
  coord %<>% as.data.frame()
  date_predict <- stats::predict.lm(fit, coord, se.fit = TRUE,
                                    interval = "confidence", level = level)
  fit_results <- data.frame(
    rownames(coord),
    date_predict$fit,
    date_predict$se.fit,
    row.names = NULL
  )
  colnames(fit_results) <- c("id", "estimation", "earliest", "latest", "error")

  return(list(fit = fit_results,
              residual = date_predict$residual.scale))
}

#' Predict accumulation dates
#'
#' @param x A \code{\link{numeric}} matrix of count data.
#' @param event A \code{\link{numeric}} matrix of predicted event dates of
#'  fabrics.
#' @param range A lenth-two \code{\link{numeric}} vector containing the minimum
#'  and maximum of all the predicted event dates of assemblages.
#' @return A two columns \code{\link{data.frame}} giving the point estimate of
#' accumulation dates of archaeological assemblages and an identifier to link
#'  each row to an assemblage.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
predictAccumulation <- function(x, event, range) {
  col_dates <- event$fit[, "estimation"]
  col_errors <- event$fit[, "error"]
  date_range <- seq(from = min(range), to = max(range), length.out = 500)

  # Gaussian mixture
  freq <- x / rowSums(x)
  col_density <- mapply(function(mean, sd, x) { stats::dnorm(x, mean, sd) },
                        mean = col_dates, sd = col_errors,
                        MoreArgs = list(x = date_range), SIMPLIFY = TRUE)

  acc_density <- apply(X = freq, MARGIN = 1, FUN = function(x, density) {
    colSums(t(density) * as.numeric(x))
  }, density = col_density)

  # Point estimate of accumulation time
  ## Weighted sum of the fabric dates
  acc_mean <- apply(
    X = x, MARGIN = 1,
    FUN = function(weights, dates) {
      stats::weighted.mean(x = dates, w = weights)
    }, dates = col_dates
  )
  ## Median
  # acc_median <- apply(X = acc_density, MARGIN = 2, FUN = function(x, range) {
  #   cumulative_sum <- cumsum(x)
  #   index <- cumulative_sum / max(cumulative_sum)
  #   range[which.max(index >= 0.5)]
  # }, range = date_range)

  acc_date <- data.frame(id = names(acc_mean), date = acc_mean,
                         row.names = NULL)

  return(acc_date)
}

#' Bootstrap resampling of assemblages
#'
#' @param x An \eqn{m \times p}{m x p} \code{\link{numeric}} matrix of count data.
#' @param model An object of class \code{\link[stats]{lm}}.
#' @param margin A non-negative \code{\link{integer}} giving the subscripts
#'  which the rearrangement will be applied over: \code{1} indicates rows,
#'  \code{2} indicates columns.
#' @param n A non-negative \code{\link{integer}} giving the number of partial
#'  bootstrap replications.
#' @param keep An \code{\link{integer}} giving the number of CA factors to keep
#'  for new data prediction.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
#' @return A six columns \code{\link{data.frame}} giving the boostrap
#'  distribution statistics for each replicated sample (in rows) with the
#'  following columns:
#'  \describe{
#'   \item{id}{An identifier.}
#'   \item{min}{Minimum value.}
#'   \item{Q05}{Sample quantile to 0.05 probability.}
#'   \item{mean}{Mean value.}
#'   \item{Q95}{Sample quantile to 0.95 probability.}
#'   \item{max}{Maximum value.}
#'  }
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
bootDate <- function(x, model, margin = 1, n = 1000, keep = ncol(x),
                     level = 0.95, ...) {
  # Validation
  margin <- as.integer(margin)
  n <- as.integer(n)
  keep <- as.integer(keep)
  level <- as.numeric(level)

  # CA on the whole dataset
  axes <- min(dim(x))
  results_CA <- FactoMineR::CA(x, ncp = axes, graph = FALSE, ...)
  svd <- if (margin == 1) results_CA$svd$V else results_CA$svd$U

  # Compute date event statistics for each replicated sample
  computeStats <- function(x, n, svd, keep, model, level) {
    # n random replicates
    # replicated <- sample(x = n, size = sum(x), prob = x)
    replicated <- stats::rmultinom(n = n, size = sum(x), prob = x)
    # Compute new CA coordinates
    coords <- crossprod(replicated / colSums(replicated), svd)
    coords <- coords[, keep]
    # Workaround: same colnames as FactoMineR results used to build the model
    colnames(coords) <- paste("Dim", keep, sep = " ")
    # Gaussian multiple linear regression model
    event <- predictEvent(model, coords, level)$fit[, "estimation"]
    Q <- stats::quantile(event, probs = c(0.05, 0.95), names = FALSE)
    distrib <- cbind(min(event), Q[1], mean(event), Q[2], max(event))
    return(distrib)
  }

  boot <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbapply(X = x, MARGIN = margin,
                     FUN = computeStats,
                     n = n, svd = svd, keep = keep,
                     model = model, level = level)
  } else {
    apply(X = x, MARGIN = margin,
          FUN = computeStats,
          n = n, svd = svd, keep = keep,
          model = model, level = level)
  }

  boot_event <- data.frame(colnames(boot), t(boot), row.names = NULL)
  colnames(boot_event) <- c("id", "min", "Q05", "mean", "Q95", "max")
  return(boot_event)
}

#' Jackknife fabrics
#'
#' @param x An \eqn{m \times p}{m x p} \code{\link{numeric}} matrix of count
#'  data.
#' @param date A list of \code{\link{numeric}} values giving the known dates used
#'  for the linear model fitting.
#' @param model A \code{\link[stats:lm]{linear model}} in which coefficients will
#'  be replaced by the jackknife estimates.
#' @param keep An \code{\link{integer}} giving the number of CA factors to keep
#'  for new data prediction.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
#' @return A five columns \code{\link{data.frame}} giving the predicted
#'  event dates, the corresponding confidence interval boundaries, standard
#'  error of the predicted dates and an identifier.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
jackDate <- function(x, dates, model, keep = ncol(x), level = 0.95, ...) {
  # Validation
  keep <- as.integer(keep)
  level <- as.numeric(level)

  # CA on the whole dataset
  axes <- min(dim(x))
  results_CA <- FactoMineR::CA(x, ncp = axes, graph = FALSE, ...)

  computeCoef <- function(i, data, dates, keep, level) {
    # Removing a column may lead to rows filled only with zeros
    # We need to remove such rows to compute CA
    zero <- which(rowSums(data[, -i]) == 0)
    sampled <- if (length(zero) != 0) data[-zero, -i] else data[, -i]
    # Compute CA
    axes <- min(dim(sampled))
    results_CA <- FactoMineR::CA(sampled, ncp = axes, graph = FALSE, ...)
    row_coord <- as.data.frame(results_CA$row$coord)
    # Gaussian multiple linear regression model
    contexts <- merge(dates, row_coord[, keep], by.x = "id", by.y = "row.names")
    contexts %<>% as.data.frame() %>% dplyr::select(-1)
    fit <- stats::lm(date ~ ., data = contexts)
    # Return model coefficients
    return(stats::coef(fit))
  }

  jack_coef <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply(X = 1:ncol(x), FUN = computeCoef,
                      data = x, dates = dates, keep = keep, level = level)
  } else {
    lapply(X = 1:ncol(x), FUN = computeCoef,
           data = x, dates = dates, keep = keep, level = level)
  }

  # Predict event date for each context
  jack_mean <- apply(X = do.call(rbind, jack_coef), MARGIN = 2, FUN = mean)
  jack_fit <- model
  jack_fit$coefficients <- jack_mean
  jack_event <- predictEvent(jack_fit, results_CA$row$coord, level)$fit

  return(jack_event)
}
