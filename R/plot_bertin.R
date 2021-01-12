# PLOT BERTIN
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_bar
#' @aliases plot_bertin,CountMatrix-method
setMethod(
  f = "plot_bertin",
  signature = signature(object = "CountMatrix"),
  definition = function(object, threshold = NULL, scale = NULL) {
    # Prepare data
    data <- prepare_bertin(object, threshold = threshold, scale = scale)

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$case, y = .data$value)
    aes_col <- if (is.null(threshold)) {
      NULL
    } else {
      ggplot2::aes(fill = .data$threshold)
    }

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$type)) +
      ggplot2::geom_col(mapping = aes_col, colour = "black") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.text.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
        strip.text.x = ggplot2::element_text(angle = 90, hjust = 0,
                                             vjust = 0.5),
        strip.background = ggplot2::element_rect(fill = "white")
      ) +
      ggplot2::labs(x = "Case", y = "Frequency", fill = "Threshold")
  }
)

# Prepare data for Bertin plot
# Must return a data.frame
prepare_bertin <- function(object, threshold = NULL, scale = NULL) {
  # Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_long(object, factor = TRUE)

  # Scale variables
  if (is.function(scale)) {
    data <- by(
      data,
      INDICES = data$type,
      FUN = function(x, fun) {
        x$value <- fun(x$value)
        x
      },
      fun = scale
    )
    data <- do.call(rbind.data.frame, data)
  }

  # Compute threshold, if any
  if (is.function(threshold)) {
    data <- by(
      data,
      INDICES = data$type,
      FUN = function(x, fun) {
        x$threshold_value <- fun(x$value)
        x
      },
      fun = threshold
    )
    data <- do.call(rbind.data.frame, data)
    threshold <- ifelse(data$value > data$threshold_value, "above", "below")
    data <- cbind.data.frame(threshold, data)
  }
  data
}

