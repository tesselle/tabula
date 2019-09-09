#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname test
#' @aliases test,CountMatrix-method
setMethod(
  f = "test",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("FIT"), simplify = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    simplify <- as.logical(simplify)[1L]

    # Get time coordinates
    time <- get_dates(object)[["value"]]
    if (isEmpty(time))
      stop("No dates were found!", call. = FALSE)

    results <- switch (
      method,
      FIT = testFIT(object, time, roll = FALSE)[[1L]],
      stop(sprintf("There is no such method: %s.", method), call. = FALSE)
    )

    # Check results
    failed <- apply(X = results, MARGIN = 1, FUN = anyNA)
    if (any(failed)) {
      warning(sum(failed), " elements were skipped:\n",
              paste0("* ", rownames(results)[failed], collapse = "\n"),
              call. = FALSE)
      results <- results[!failed, ]
    }

    if (!simplify)
      results <- split(x = results, f = rownames(results))
    results
  }
)

# ==============================================================================
testFIT <- function(x, time, roll = FALSE, window = 3, ...) {
  # Validation
  checkType(x, expected = "numeric")
  checkType(time, expected = "numeric")
  roll <- as.logical(roll)[1L]
  window <- as.integer(window)[1L]

  # Prepare data
  # Compute frequency as ratio of count of type of interest to all other types
  count_others <- lapply(
    X = seq_len(ncol(x)),
    FUN = function(i, data) { rowSums(data[, -i]) },
    data = x
  )
  freq <- x / do.call(cbind, count_others)

  # Compute test
  if (!roll) {
    roll_list <- list(seq_len(nrow(freq)))
  } else {
    roll_index <- roll(freq, window = window)
    roll_list <- split(x = roll_index[["i"]], f = roll_index[["w"]])
  }
  roll_freq <- lapply(X = roll_list, FUN = function(i, x) x[i, ], x = freq)
  roll_time <- lapply(X = roll_list, FUN = function(i, x) x[i], x = time)

  results <- mapply(
    FUN = function(f, t, ...) {
      fit <- apply(
        X = f,
        MARGIN = 2,
        FUN = function(v, t, ...) {
          tryCatch(error = function(cnd) c(t = NA, p.value = NA),
                   { FIT(v, t, ...) })
        },
        t = t, ...
      )
      t(fit)
    },
    f = roll_freq, t = roll_time,
    SIMPLIFY = FALSE
  )
  results
}

#' Frequency Increment Test (FIT)
#'
#' @param v A \code{\link{numeric}} vector of frequencies.
#' @param t A \code{\link{numeric}} vector of time coordinates.
#' @param ... Extra parameter passed to \code{\link[=stats]{t.test}}.
#' @return A \code{\link{numeric}} vector containing the following components:
#'  \describe{
#'   \item{t}{the value of the test statistic.}
#'   \item{p.value}{the p-value for the test.}
#'  }
#' @author N. Frerebeau
#' @references
#'  Feder, A. F., Kryazhimskiy, S. & Plotkin, J. B. (2014). Identifying
#'  Signatures of Selection in Genetic Time Series. \emph{Genetics}, 196(2),
#'  509-522.
#'  DOI: \href{https://doi.org/10.1534/genetics.113.158220}{10.1534/genetics.113.158220}.
# @examples
# ## Data from Feder et al. 2014 (table S2)
# FIT(
#   v = c(8/115, 34/64, 84/102, 73/93, 98/105, 97/99, 97/98),
#   t = c(415, 505, 585, 665, 745, 825, 910)
# )
#' @keywords internal
FIT <- function(v, t, ...) {
  # Validation
  checkType(v, expected = "numeric")
  checkType(t, expected = "numeric")
  if (length(v) != length(t))
    stop("`v` and `t` must have the same length.", call. = FALSE)
  # Remove zeros
  index <- v > 0
  v <- v[index]
  t <- t[index]
  if (length(t) < 3)
    stop("A minimum of three time points is needed.", call. = FALSE)

  # Rescaled allele-frequency increments
  Y <- diff(v, lag = 1) /
    sqrt(2 * utils::head(v, -1) * (1 - utils::head(v, -1)) * diff(t, lag = 1))
  # Statistics
  t_test <- stats::t.test(Y, ...)
  c(t = as.numeric(t_test$statistic), p.value = t_test$p.value)
}
