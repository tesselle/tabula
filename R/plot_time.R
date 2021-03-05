# PLOT LINE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_date
#' @aliases plot_time,CountMatrix,numeric-method
setMethod(
  f = "plot_time",
  signature = signature(object = "CountMatrix", dates = "numeric"),
  definition = function(object, dates, facet = FALSE) {
    ## Validation
    if (length(dates) != nrow(object))
      stop(sprintf("%s must be of length %d; not %d.",
                   sQuote("dates"), nrow(object), length(dates)), call. = FALSE)

    ## Prepare data
    data <- prepare_time(object, dates)

    ## ggplot
    if (facet) {
      facet <- ggplot2::facet_wrap(
        facets = ggplot2::vars(.data$column),
        scales = "free_y"
      )
      aes_plot <- ggplot2::aes(x = .data$x, y = .data$y)
    } else {
      facet <- NULL
      aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, colour = .data$column)
    }

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(name = "Time") +
      ggplot2::scale_y_continuous(name = "Frequency") +
      facet
  }
)

#' @export
#' @rdname test_fit
#' @aliases plot_time,IncrementTest,missing-method
setMethod(
  f = "plot_time",
  signature = signature(object = "IncrementTest", dates = "missing"),
  definition = function(object, level = 0.95, roll = FALSE, window = 3) {

    alpha <- 1 - level

    ## Prepare data
    counts <- object[["data"]]
    dates <- object[["dates"]]
    data <- prepare_time(counts, dates)

    signature_fit <- as.data.frame(object)
    signature_fit$signature <- ifelse(signature_fit$p.value <= alpha,
                                      "selection", "neutral")

    data <- merge(x = data, y = signature_fit, by.x = "column", by.y = 0,
                  all.x = TRUE, all.y = FALSE)

    if (roll) {
      roll_fit <- testFIT(counts, dates, roll = roll, window = window)

      roll_fit <- do.call(rbind.data.frame, roll_fit)
      roll_fit$signature <- roll_fit$p.value <= alpha

      data <- merge(x = data, y = roll_fit, by = c("column", "dates"),
                    all.x = TRUE, all.y = FALSE, sort = TRUE,
                    suffixes = c("","_roll"))

      data <- by(
        data,
        INDICES = data$column,
        FUN = function(x, half_window) {
          x$signature_sub <- vapply(
            X = seq_along(x$signature_roll),
            FUN = function(x, y, k) {
              max <- length(y)
              lower <- x - k
              lower[lower < 1] <- 1
              upper <- x + k
              upper[upper > max] <- max
              any(y[lower:upper], na.rm = TRUE)
            },
            FUN.VALUE = logical(1),
            y = x$signature_roll,
            k = half_window
          )
          x
        },
        half_window = (window - 1) / 2
      )
      data <- do.call(rbind.data.frame, data)

      gg_roll <- ggplot2::geom_line(
        data = data[data$signature_sub, ],
        mapping = ggplot2::aes(group = .data$column),
        size = 5, colour = "grey80", lineend = "round"
      )
    } else {
      gg_roll <- NULL
    }

    ## ggplot
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$signature) +
      gg_roll +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(name = "Time") +
      ggplot2::scale_y_continuous(name = "Frequency") +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(.data$column),
        scales = "free_y"
      )
  }
)

## Prepare data for time plot
## Must return a data.frame
prepare_time <- function(object, dates) {
  ## Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_long(object, factor = TRUE)

  data$x <- data$dates <- dates
  data$y <- data$value

  ## Remove zeros in case of log scale
  data <- data[data$value > 0, ]

  return(data)
}
