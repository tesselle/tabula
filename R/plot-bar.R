# PLOT BAR
#' @include AllGenerics.R AllClasses.R plot-prepare.R
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
    aes_plot <- ggplot2::aes(x = .data$case, y = .data$frequency)
    aes_col <- if (is.null(threshold)) {
      NULL
    } else {
      ggplot2::aes(fill = .data$threshold)
    }

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_col(mapping = aes_col, colour = "black") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$type), scales = "free_y") +
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

#' @export
#' @rdname plot_bar
#' @aliases plot_ford,CountMatrix-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "CountMatrix"),
  definition = function(object, EPPM = FALSE) {
    # Prepare data
    data <- prepare_ford(object, EPPM = EPPM)

    # ggplot
    # A function that given the scale limits returns a vector of breaks
    scale_breaks <- function(x) {
      if (max(x) >= 0.2) {
        seq(-4, 4, by = 1) * 0.10
      } else {
        seq(-1, 1, by = 1) * 0.05
      }
    }
    # A function that takes the breaks as input and returns labels as output
    scale_labels <- function(x) {
      labs <- scales::percent(x = abs(x), accuracy = 1)
      labs[ceiling(length(x) / 2)] <- ""
      labs
    }

    aes_plot <- ggplot2::aes(x = .data$case, y = .data$data)
    aes_col <- if (EPPM) ggplot2::aes(fill = .data$threshold) else NULL
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_col(mapping = aes_col, width = 1,
                        position = ggplot2::position_stack(reverse = FALSE)) +
      ggplot2::facet_grid(. ~ type, scales = "free_x", space = "free_x") +
      ggplot2::scale_y_continuous(breaks = scale_breaks, labels = scale_labels,
                                  expand = c(0, 0.025)) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(angle = 90, hjust = 0,
                                             vjust = 0.5),
        strip.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::labs(x = "Case", y = "Frequency", fill = "Value") +
      ggplot2::coord_flip()
  }
)
