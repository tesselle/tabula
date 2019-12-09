# PLOT LINE
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plot_date
#' @aliases plot_time,CountMatrix-method
setMethod(
  f = "plot_time",
  signature = signature(object = "CountMatrix"),
  definition = function(object, highlight = NULL, level = 0.95,
                        roll = FALSE, window = 5, facet = TRUE, ...) {
    # Validation
    highlight <- highlight %||% "none"
    highlight <- match.arg(highlight, choices = c("none", "FIT"),
                           several.ok = FALSE)
    if (highlight == "FIT") facet <- TRUE # Override default
    alpha <- 1 - level

    # Prepare data
    gg_roll <- NULL
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object)
    row_names <- factor(x = row_names, levels = unique(row_names))
    # Get number of cases
    n <- length(row_names)
    # Get time coordinates
    time <- get_dates(object)[["value"]]
    if (length(time) == 0)
        stop("Time coordinates are missing!", call. = FALSE)

    data_stacked <- utils::stack(as.data.frame(object))
    data <- cbind.data.frame(row_names, time, data_stacked)
    colnames(data) <- c("case", "time", "frequency", "type")
    # Preserves the original ordering of the columns
    data$type <- factor(data$type, levels = unique(data$type))

    # Remove zeros in case of log scale
    data <- data[data$frequency > 0, ]

    if (highlight == "FIT") {
      signature <- as.data.frame(testFIT(object, time, roll = FALSE)[[1L]])
      signature <- cbind.data.frame(
        type = factor(rownames(signature), levels = unique(rownames(signature))),
        signature = ifelse(signature$p.value <= alpha, "selection", "neutral")
      )

      data <- merge(x = data, y = signature, by = c("type"),
                    all.x = TRUE, all.y = FALSE)

      if (roll) {
        k <- (window - 1) / 2
        fit <- testFIT(object, time, roll = roll, window = window)
        fit <- lapply(
          X = fit,
          FUN = function(x) {
            row_names <- factor(rownames(x), levels = unique(rownames(x)))
            cbind.data.frame(type = row_names, x)
          })
        id <- rep(names(fit), times = vapply(fit, nrow, numeric(1)))
        fit <- do.call(rbind.data.frame, fit)
        fit <- cbind.data.frame(
          type = fit$type,
          sub_signature = ifelse(fit$p.value <= alpha, "selection", "neutral"),
          time = time[as.integer(id)]
        )

        data <- merge(x = data, y = fit, by = c("type", "time"),
                      all.x = TRUE, all.y = FALSE)
        data <- data[order(data$type, data$time), ]
        data <- by(
          data,
          INDICES = data$type,
          FUN = function(x, k) {
            sub_signature <- ifelse(
              vapply(
                X = seq_along(x$sub_signature),
                FUN = function(x, var, k) {
                  max <- length(var)
                  lower <- x - k
                  lower[lower < 1] <- 1
                  upper <- x + k
                  upper[upper > max] <- max
                  any(var[lower:upper] == "selection")
                },
                FUN.VALUE = logical(1),
                var = x$sub_signature, k = k
              ),
              "selection", "neutral")
            x$sub_signature <- sub_signature
            x
          },
          k = k
        )
        data <- do.call(rbind.data.frame, data)

        data_sub <- stats::na.omit(data[data$sub_signature == "selection", ])
        gg_roll <- ggplot2::geom_line(
          data = data_sub,
          mapping = ggplot2::aes(group = data_sub$type),
          size = 5, colour = "grey80", lineend = "round"
        )
      }
    }

    data <- data[order(data$type, data$time), ]

    # ggplot
    colour <- ifelse(highlight == "FIT", "signature", "type")
    aes_plot <- ggplot2::aes(x = .data$time, y = .data$frequency,
                             colour = .data[[colour]])
    if (facet) {
      facet <- ggplot2::facet_wrap(ggplot2::vars(.data$type), scales = "free_y")
      if (highlight != "FIT") {
        aes_plot <- ggplot2::aes(x = .data$time, y = .data$frequency)
      }
    } else {
      facet <- NULL
    }

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      gg_roll + ggplot2::geom_point() + ggplot2::geom_line() + facet +
      ggplot2::labs(x = "Time", y = "Frequency", colour = colour)
  }
)

#' Indices of a rolling window
#'
#' @param x An object.
#' @param window A \code{\link{integer}} scalar giving the window size.
#' @return A \code{\link{list}} with the following components:
#'  \describe{
#'   \item{i}{A \code{\link{integer}} vector of indices.}
#'   \item{w}{A \code{\link{integer}} vector of indices giving the window
#'   number.}
#'   \item{m}{A \code{\link{integer}} vector of indices giving the indice of
#'   the window mid-point.}
#'  }
#' @keywords internal
#' @noRd
roll <- function(x, window = 3, simplify = FALSE) {
  # Validation
  if (window %% 2 == 0)
    stop("`window` must be an odd integer.", call. = FALSE)

  if (is.matrix(x) || is.data.frame(x)) {
    n <- nrow(x)
  } else {
    n <- length(x)
  }
  i <- seq_len(n) # Indices of the rows
  # Matrix of rolling-window indices of length w
  w <- stats::embed(i, window)[, window:1]
  inds <- i[c(t(w))] # Flatten indices
  # Window mid-point
  m <- w[, ceiling(window / 2)]
  list(i = inds, w = rep(m, each = window))
}
