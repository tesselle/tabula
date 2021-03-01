# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname date_mcd
#' @aliases date_mcd,CountMatrix,numeric-method
setMethod(
  f = "date_mcd",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates, errors = NULL, ...) {
    ## Validation
    if (length(dates) != ncol(object))
      stop(sprintf("%s must be of length %d; not %d.", sQuote("dates"),
                   ncol(object), length(dates)), call. = FALSE)
    if (!is.null(errors) & (length(errors) != ncol(object)))
      stop(sprintf("%s must be of length %d; not %d.", sQuote("errors"),
                   ncol(object), length(errors)), call. = FALSE)

    ## Calculate MCD
    mcd_dates <- apply(
      X = object,
      MARGIN = 1,
      FUN = mcd,
      dates = dates
    )

    ## Calculate errors
    if (!is.null(errors)) {
      errors_mcd <- sqrt(rowSums((object * errors)^2))
    } else {
      errors_mcd <- rep(0, length.out = length(mcd_dates))
    }

    .DateMCD(
      data = as.matrix(object),
      dates = dates,
      mcd_values = mcd_dates,
      mcd_errors = errors_mcd
    )
  }
)

#' @export
#' @rdname date_mcd
#' @aliases bootstrap_mcd,CountMatrix,numeric-method
setMethod(
  f = "bootstrap_mcd",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates, probs = c(0.05, 0.95), n = 1000) {
    results <- apply(
      X = object,
      MARGIN = 1,
      FUN = stats_bootstrap,
      do = mcd,
      probs = probs,
      n = n,
      dates = dates
    )
    as.data.frame(t(results))
  }
)

#' Mean Ceramic Date
#'
#' @param counts A \code{\link{numeric}} vector.
#' @param dates A \code{\link{numeric}} vector.
#' @return A \code{\link{numeric}} value.
#' @keywords internal
#' @noRd
mcd <- function(counts, dates) {
  ## Calculate relative frequencies
  freq <- counts / sum(counts, na.rm = TRUE)

  ## Calculate date
  dates_mcd <- sum(freq * dates, na.rm = TRUE)

  dates_mcd
}
