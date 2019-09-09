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
    row_names <- rownames(object) %>% factor(levels = unique(.))
    # Get number of cases
    n <- length(row_names)
    # Get time coordinates
    time <- get_dates(object)[, 1, drop = TRUE]
    if (isEmpty(time))
        stop("Time coordinates are missing!", call. = FALSE)

    data <- object %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names,
                    time = time) %>%
      tidyr::gather(key = "type", value = "frequency", -.data$case, -.data$time,
                    factor_key = TRUE) %>%
      dplyr::filter(.data$frequency > 0) # Remove zeros in case of log scale

    if (highlight == "FIT") {
      signature <- testFIT(object, time, roll = FALSE)[[1L]] %>%
        as.data.frame() %>%
        dplyr::transmute(
          type = factor(rownames(.), levels = unique(rownames(.))),
          signature = ifelse(.data$p.value <= alpha, "selection", "neutral")
        )

      data %<>% dplyr::left_join(y = signature, by = c("type"))

      if (roll) {
        k <- (window - 1) / 2
        fit <- object %>%
          testFIT(time, roll = roll, window = window) %>%
          lapply(FUN = function(x) {
            x %>%
              as.data.frame() %>%
              dplyr::mutate(
                type = factor(rownames(.), levels = unique(rownames(.)))
              )
          }) %>%
          dplyr::bind_rows(.id = "w") %>%
          dplyr::transmute(
            type = .data$type,
            sub_signature = ifelse(.data$p.value <= alpha,
                                   "selection", "neutral"),
            time = time[as.integer(.data$w)]
          )

        data %<>% dplyr::left_join(y = fit, by = c("type", "time")) %>%
          dplyr::arrange(.data$type, .data$time) %>%
          dplyr::group_by(.data$type) %>%
          dplyr::mutate(
            sub_signature = ifelse(
              vapply(
                X = seq_along(.data$sub_signature),
                FUN = function(x, var, k) {
                  max <- length(var)
                  lower <- x - k
                  lower[lower < 1] <- 1
                  upper <- x + k
                  upper[upper > max] <- max
                  any(var[lower:upper] == "selection")
                },
                FUN.VALUE = logical(1),
                var = .data$sub_signature, k = k
              ),
              "selection", "neutral")
          ) %>%
          dplyr::ungroup()

        gg_roll <- data %>%
          dplyr::filter(.data$sub_signature == "selection") %>%
          ggplot2::geom_line(mapping = ggplot2::aes(group = .data$type),
                             size = 5, colour = "grey80", lineend = "round")
      }
    }

    data %<>% dplyr::arrange(.data$type, .data$time)

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
  if (!isOdd(window))
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
