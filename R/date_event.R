# DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname event
#' @aliases date_event,CountMatrix,numeric-method
setMethod(
  f = "date_event",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates, cutoff = 90, ...) {
    ## Validation
    cutoff <- as.integer(cutoff)
    if (cutoff < 50)
      stop("Cutoff value is below 50%, you can't be serious.", call. = FALSE)

    ## Correspondance analysis
    results_CA <- dimensio::ca(object, ...)
    eig <- dimensio::get_eigenvalues(results_CA)
    keep_dim <- which(eig[, 3] <= cutoff)

    row_coord <- dimensio::get_coordinates(results_CA, margin = 1)
    row_coord <- row_coord[, keep_dim]

    ## CA computation may rise error (if rows/columns filled only with zeros)
    ## FIXME: we may need to remove corresponding dates

    ## Event date
    contexts <- bind_by_names(row_coord, dates)
    colnames(contexts)[1] <- "date"

    ## Gaussian multiple linear regression model
    fit <- stats::lm(date ~ ., data = contexts, na.action = stats::na.omit)

    .DateModel(
      data = object,
      dates = contexts$date,
      model = fit,
      cutoff = cutoff,
      dimension = keep_dim
    )
  }
)

#' @export
#' @rdname event
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
#' @rdname event
#' @aliases predict_event,DateModel,CountMatrix-method
setMethod(
  f = "predict_event",
  signature = signature(object = "DateModel", data = "CountMatrix"),
  definition = function(object, data, level = 0.95, ...) {

    fit_model <- object[["model"]]

    ## Correspondance analysis
    results_CA <- dimensio::ca(data)
    row_coord <- dimensio::get_coordinates(results_CA, margin = 1)
    col_coord <- dimensio::get_coordinates(results_CA, margin = 2)

    ## Predict event date for each context
    row_event <- predict_events(fit_model, row_coord, level = level)
    ## Predict event dates for each fabric
    col_event <- predict_events(fit_model, col_coord, level = level)

    # FIXME: error propagation
    # TODO: check predicted dates consistency

    # Accumulation time point estimate
    date_range <- range(row_event[, c("lower", "upper")])
    acc_estimate <- predict_accumulation(data, col_event, date_range)

    .DateEvent(
      data = as.matrix(data),
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
