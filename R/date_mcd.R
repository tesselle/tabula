# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname date
#' @aliases date_mcd,CountMatrix,numeric-method
setMethod(
  f = "date_mcd",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates, errors = NULL,
                        level = 0.95, n = 1000, ...) {
    # Validation
    if (length(dates) != ncol(object))
      stop(sprintf("%s must be of length %d; not %d.", sQuote("dates"),
                   ncol(object), length(dates)), call. = FALSE)
    if (!is.null(errors) & (length(errors) != ncol(object)))
      stop(sprintf("%s must be of length %d; not %d.", sQuote("errors"),
                   ncol(object), length(errors)), call. = FALSE)

    mcd <- function(count, dates, errors = NULL) {
      # Build a matrix of dates
      i <- nrow(count)
      j <- ncol(count)
      dates <- matrix(data = dates, nrow = i, ncol = j, byrow = TRUE)
      # Calculate relative frequencies
      freq <- count / rowSums(count)
      # Calculate date
      dates_mcd <- list(date = rowSums(freq * dates))
      # Calculate errors
      if (!is.null(errors)) {
        dates_mcd <- append(dates_mcd,
                            list(error = sqrt(rowSums((freq * errors)^2))))
      }
      dates_mcd
    }
    # Calculate MCD
    mcd_dates <- mcd(object, dates, errors)

    # Bootstrap confidence interval
    mcd_errors <- apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, dates, level, n) {
        sim <- stats::rmultinom(n, size = sum(x), prob = x)
        sim <- t(sim)
        temp <- mcd(sim, dates)[[1L]]
        ci <- try(stats::t.test(temp, conf.level = level)$conf.int,
                  silent = TRUE)
        if (inherits(ci, "try-error")) c(NA_real_, NA_real_) else ci
      },
      dates, level, n
    )
    rownames(mcd_errors) <- c("lower", "upper")

    results <- cbind.data.frame(mcd_dates, t(mcd_errors))
    rownames(results) <- rownames(object)
    results
  }
)
