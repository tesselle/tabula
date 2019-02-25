# DATE MODEL
#' @include AllGenerics.R AllClasses.R
NULL

# Predict event dates
#
# @param fit A \code{\link[stats:lm]{multiple linear model}}.
# @param coord A \code{\link{numeric}} matrix giving the coordinates in CA
#  space.
# @param level A length-one \code{\link{numeric}} vector giving the
#  confidence level.
# @return
#  A list with the following components:
#  \describe{
#   \item{fit}{A five columns \code{\link{data.frame}} giving the predicted
#   event dates, the corresponding confidence interval boundaries, standard
#   error of the predicted dates and an identifier.}
#   \item{residual}{Residual standard deviations.}
#  }
# @author N. Frerebeau
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

# Predict accumulation dates
#
# @param x A \code{\link{numeric}} matrix of count data.
# @param event A \code{\link{numeric}} matrix of predicted event dates of
#  fabrics.
# @param range A lenth-two \code{\link{numeric}} vector containing the minimum
#  and maximum of all the predicted event dates of assemblages.
# @return A two columns \code{\link{data.frame}} giving the point estimate of
#  accumulation dates of archaeological assemblages and an identifier to link
#  each row to an assemblage.
# @author N. Frerebeau
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

#' @export
#' @rdname date
#' @aliases dateEvent,CountMatrix-method
setMethod(
  f = "dateEvent",
  signature = signature(object = "CountMatrix"),
  definition = function(object, dates, level = 0.95, cutoff = 90,
                        verbose = FALSE, ...) {
    # Validation
    if (!is.list(dates)) stop("A list of dates is expected.")
    if (cutoff < 50) stop("Cutoff value is below 50%, you can't be serious.")
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
    if (verbose) { FactoMineR::summary.CA(results_CA) }

    # Event date
    ## Gaussian multiple linear regression model
    contexts <- merge(dates, row_coord, by.x = "id", by.y = "row.names")
    contexts %<>% as.data.frame() %>% dplyr::select(-1)
    fit <- stats::lm(date ~ ., data = contexts)
    if (verbose) { print(stats::summary.lm(fit)) }

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

    ## Check predicted dates
    # row_predicted <- merge(dates, row_event$fit, by = "row.names")
    # apply(
    #   X = row_predicted, MARGIN = 1,
    #   FUN = function(x, level) {
    #     name <- x["id"]
    #     date <- x["date"]
    #     start <- x["earliest"]
    #     stop <- x["latest"]
    #     if (date < start | date > stop) {
    #       warning(paste("Model fitting: context ", name,
    #                     " does not lie in the predicted ", level * 100,
    #                     "% confidence interval.", sep = ""), call. = FALSE)
    #     }
    #   }, level
    # )

    # Accumulation time point estimate
    date_range <- range(row_event$fit[, c("earliest", "latest")])
    acc_estimate <- predictAccumulation(x = counts, event = col_event,
                                        range = date_range)

    methods::new(
      "DateModel",
      counts = counts,
      dates = dates,
      level = level,
      model = fit,
      residual = row_event$residual,
      rows = row_event$fit,
      columns = col_event$fit,
      accumulation = acc_estimate
    )
  }
)
