# MEAN CERAMIC DATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname date_mcd
#' @aliases date_mcd,AbundanceMatrix,numeric-method
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
    mcd_dates <- mcd(object, dates, errors)

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

mcd <- function(count, dates, errors = NULL) {
  ## Build a matrix of dates
  i <- nrow(count)
  j <- ncol(count)
  dates <- matrix(data = dates, nrow = i, ncol = j, byrow = TRUE)

  ## Calculate relative frequencies
  freq <- count / rowSums(count, na.rm = TRUE)

  ## Calculate date
  dates_mcd <- rowSums(freq * dates, na.rm = TRUE)

  return(dates_mcd)
}
