#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname test
#' @aliases test_fit,CountMatrix-method
setMethod(
  f = "test_fit",
  signature = signature(object = "CountMatrix"),
  definition = function(object, simplify = FALSE, ...) {
    # Validation
    simplify <- as.logical(simplify)[1L]

    # Get time coordinates
    time <- get_dates(object)[["value"]]
    if (is_empty(time))
      stop("No dates were found!", call. = FALSE)

    results <- testFIT(object, time, roll = FALSE)[[1L]]

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
    return(results)
  }
)
#' @export
#' @rdname test
#' @aliases test_diversity,CountMatrix-method
setMethod(
  f = "test_diversity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, adjust = "holm", ...) {
    # Calculate the number of individuals
    N <- apply(X = object, MARGIN = 1, FUN = sum)
    # Calculate Shannon diversity
    H <- apply(X = object, MARGIN = 1, FUN = diversityShannon, ...)
    # Calculate Shannon variance
    V <- apply(X = object, MARGIN = 1, FUN = varianceShannon, ...)
    # Get the names of the assemblages
    row_names <- rownames(object)
    if (length(row_names) != 0) {
      row_names <- factor(row_names, levels = unique(row_names))
    } else {
      row_names <- factor(seq_len(nrow(object)))
    }
    # Compute t test
    compare <- function(i, j) {
      tt <- (H[i] - H[j]) / sqrt(V[i] + V[j])
      df <- (V[i] + V[j])^2 / sum(V[c(i, j)]^2 / N[c(i, j)])
      2 * (1 - stats::pt(q = abs(tt), df = df))
    }
    result <- stats::pairwise.table(
      compare.levels = compare,
      level.names = row_names,
      p.adjust.method = adjust
    )
    return(result)
  }
)

# ==============================================================================
testFIT <- function(x, time, roll = FALSE, window = 3, ...) {
  # Validation
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
  return(results)
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
#' @keywords internal
#' @noRd
FIT <- function(v, t, ...) {
  # Validation
  if (!is.numeric(v) || !is.numeric(t))
    stop("`v` and `t` must be of numeric type.", call. = FALSE)
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
