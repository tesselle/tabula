# DATE MODEL
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname date
#' @aliases date_mcd,CountMatrix-method
setMethod(
  f = "date_mcd",
  signature = signature(object = "CountMatrix"),
  definition = function(object, dates, errors = NULL,
                        level = 0.95, n = 1000, ...) {
    # Validation
    i <- nrow(object)
    j <- ncol(object)
    check_length(dates, expected = j)
    if (!is.null(errors) && is.numeric(errors))
      check_length(errors, expected = j)
    if (!is.null(names(dates)))
      check_names(dates, expected = colnames(object))

    mcd <- function(count, dates, errors = NULL) {
      # Build a matrix of dates
      i <- nrow(count)
      j <- ncol(count)
      dates <- matrix(data = dates, nrow = i, ncol = j, byrow = TRUE)
      # Calculate relative frequencies
      freq <- count / rowSums(count)
      # Calculate date
      mcd_dates <- rowSums(freq * dates)
      # Calculate errors
      if (!is.null(errors) && is.numeric(errors)) {
        mcd_errors <- sqrt(rowSums((freq * errors)^2))
      } else {
        mcd_errors <- rep(NA_real_, times = i)
      }
      return(list(mcd_dates, mcd_errors))
    }

    # Calculate MCD
    mcd_dates <- mcd(object, dates, errors)

    # Bootstrap confidence interval
    mcd_errors <-apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, dates, level, n) {
        sim <- stats::rmultinom(n, size = sum(x), prob = x)
        sim <- t(sim)
        temp <- mcd(sim, dates)[[1]]
        ci <- try(stats::t.test(temp, conf.level = level)$conf.int,
                  silent = TRUE)
        if (is_error(ci)) c(NA_real_, NA_real_) else ci
      },
      dates, level, n
    )

    id <- if (!is.null(rownames(object))) rownames(object) else seq_len(i)
    results <- cbind.data.frame(id, mcd_dates, t(mcd_errors),
                                stringsAsFactors = FALSE)
    rownames(results) <- NULL
    colnames(results) <- c("id", "date", "error", "lower", "upper")
    attr(results, "level") <- level
    return(results)
  }
)

#' @export
#' @rdname date
#' @aliases date_event,CountMatrix-method
setMethod(
  f = "date_event",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = 0.95, cutoff = 90, ...) {
    # Validation
    cutoff <- as.integer(cutoff)
    if (cutoff < 50)
      stop("Cutoff value is below 50%, you can't be serious.", call. = FALSE)

    # Get dates
    dates <- rownames_to_column(object@dates, factor = TRUE, id = "id")
    dates <- dates[stats::complete.cases(dates), ]

    if (nrow(dates) == 0)
      stop("No dates were found!", call. = FALSE)
    # Get count data
    counts <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    # Correspondance analysis
    axes <- min(dim(counts))
    results_CA <- ca::ca(counts, nd = axes, ...)
    eig <- results_CA$sv^2 # Eigenvalue
    var_CA <- cumsum(eig / sum(eig)) * 100 # Cumulative percentage of variance
    keep_dim <- which(var_CA <= cutoff)

    coords_CA <- ca::cacoord(results_CA, type = "principal")
    row_coord <- coords_CA$rows[, keep_dim]
    col_coord <- coords_CA$columns[, keep_dim]

    # Event date
    ## Gaussian multiple linear regression model
    contexts <- merge(dates, row_coord, by.x = "id", by.y = "row.names")
    ctxt_id <- contexts$id
    ## Remove 'id' and 'error' columns before fitting
    contexts <- contexts[, !(names(contexts) %in% c("id", "error"))]
    names(contexts)[names(contexts) == "value"] <- "date"

    ## Set rownames: we need these for refine()
    rownames(contexts) <- ctxt_id
    fit <- stats::lm(date ~ ., data = contexts)

    ## Predict event date for each context
    row_event <- predict_event(fit, row_coord, level = level)
    ## Predict event dates for each fabric
    col_event <- predict_event(fit, col_coord, level = level)

    # FIXME: error propagation
    # TODO: check predicted dates consistency

    # Accumulation time point estimate
    date_range <- range(row_event[, c("lower", "upper")])
    acc_estimate <- predict_accumulation(counts, col_event, date_range)

    DateModel(
      id = object@id,
      counts = counts,
      level = level,
      model = fit,
      rows = row_event,
      columns = col_event,
      accumulation = acc_estimate
    )
  }
)

# ==============================================================================
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
predict_event <- function(fit, data, level) {
  date_predict <- stats::predict.lm(fit, as.data.frame(data), se.fit = TRUE,
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
