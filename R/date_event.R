# DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname date
#' @aliases date_event,CountMatrix,numeric-method
setMethod(
  f = "date_event",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates, cutoff = 90, ...) {
    # Validation
    cutoff <- as.integer(cutoff)
    if (cutoff < 50)
      stop("Cutoff value is below 50%, you can't be serious.", call. = FALSE)
    if (length(dates) != nrow(object)) {
      index <- match(names(dates), rownames(object))
      if (length(index) < length(dates))
        stop(sprintf("Names of %s do not match.", sQuote("dates"),
                     nrow(object), length(dates)), call. = FALSE)
      dates_ok <- rep(NA, times = nrow(object))
      dates_ok[index] <- dates
    } else {
      dates_ok <- dates
    }

    # Correspondance analysis
    results_CA <- run_ca(object, ...)
    eig <- results_CA[["eigenvalue"]]
    keep_dim <- which(eig[, 3] <= cutoff)

    row_coord <- results_CA[["row_coordinates"]][, keep_dim]
    col_coord <- results_CA[["column_coordinates"]][, keep_dim]

    # CA computation may lead to rows/column removal (if filled only with zeros)
    # We need to remove corresponding dates
    dates_ok <- dates_ok[rowSums(object) != 0]

    # Event date
    names(dates_ok) <- rownames(row_coord)
    contexts <- cbind.data.frame(date = dates_ok, row_coord)
    ## Remove missing dates
    contexts <- contexts[!is.na(dates_ok), ]
    ## Gaussian multiple linear regression model
    fit <- stats::lm(date ~ ., data = contexts)

    .DateModel(
      id = arkhe::get_id(object),
      data = arkhe::as_matrix(object),
      dates = dates_ok,
      model = fit,
      dimension = keep_dim
    )
  }
)

#' @export
#' @rdname date
#' @aliases predict_event,DateModel,missing-method
setMethod(
  f = "predict_event",
  signature = signature(object = "DateModel", data = "missing"),
  definition = function(object, level = 0.95, ...) {
    data <- object[["data"]]
    data <- arkhe::as_count(data)
    predict_event(object = object, data = data, level = level, ...)
  }
)

#' @export
#' @rdname date
#' @aliases predict_event,DateModel,CountMatrix-method
setMethod(
  f = "predict_event",
  signature = signature(object = "DateModel", data = "CountMatrix"),
  definition = function(object, data, level = 0.95, ...) {

    fit_model <- object[["model"]]

    ## Correspondance analysis
    results_CA <- run_ca(data)
    row_coord <- results_CA[["row_coordinates"]]
    col_coord <- results_CA[["column_coordinates"]]

    ## Predict event date for each context
    row_event <- predict_events(fit_model, row_coord, level = level)
    ## Predict event dates for each fabric
    col_event <- predict_events(fit_model, col_coord, level = level)

    # FIXME: error propagation
    # TODO: check predicted dates consistency

    # Accumulation time point estimate
    counts <- arkhe::as_matrix(data)
    date_range <- range(row_event[, c("lower", "upper")])
    acc_estimate <- predict_accumulation(counts, col_event, date_range)

    .DateEvent(
      id = arkhe::get_id(data),
      data = counts,
      level = level,
      row_events = row_event,
      column_events = col_event,
      accumulation = acc_estimate
    )
  }
)

#' Predict event dates
#'
#' @param fit A \code{\link[stats:lm]{multiple linear model}}.
#' @param data A \code{\link{numeric}} matrix giving the coordinates in CA
#'  space.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @return
#'  A four columns \code{\link{numeric}} matrix giving the predicted
#'  event dates, the corresponding confidence interval boundaries and the
#'  standard error of the predicted dates.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
predict_events <- function(fit, data, level) {
  data <- as.data.frame(data)
  date_predict <- stats::predict.lm(fit, data, se.fit = TRUE,
                                    interval = "confidence", level = level)
  results <- cbind(
    date_predict$fit, # Three columns matrix: predicted value + CI boudaries
    date_predict$se.fit
  )
  rownames(results) <- rownames(data)
  colnames(results) <- c("date", "lower", "upper", "error")

  results
}

#' Predict accumulation dates
#'
#' @param count A \code{\link{numeric}} matrix of count data.
#' @param event A \code{\link{numeric}} matrix of predicted event dates of
#'  fabrics.
#' @param range A lenth-two \code{\link{numeric}} vector containing the minimum
#'  and maximum of all the predicted event dates of assemblages.
#' @return
#'  A two columns \code{\link{numeric}} matrix giving the predicted
#'  accumulation dates and the corresponding errors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
predict_accumulation <- function(count, event, range) {
  col_dates <- event[, "date", drop = TRUE]
  col_errors <- event[, "error", drop = TRUE]
  date_range <- seq(from = min(range), to = max(range), length.out = 500)

  # Point estimate of accumulation time
  ## Weighted sum of the fabric dates
  acc_mean <- apply(
    X = count, MARGIN = 1,
    FUN = function(weights, dates) {
      stats::weighted.mean(x = dates, w = weights)
    }, dates = col_dates
  )

  # FIXME: error propagation
  results <- cbind(
    date = acc_mean,
    error = rep.int(0, times = length(acc_mean))
  )
  rownames(results) <- rownames(count)
  results
}
