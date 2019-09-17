# PLOT SPOT
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,CountMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "CountMatrix"),
  definition = function(object, threshold = NULL) {
    freq <- methods::as(object, "FrequencyMatrix")
    plot_spot(freq, threshold = threshold)
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,FrequencyMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, threshold = NULL) {
    # Prepare data
    data <- prepare_spot(object, threshold)

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$type, y = .data$case)
    aes_point <- if (is.null(threshold)) {
      ggplot2::aes(size = .data$value)
    } else {
      ggplot2::aes(size = .data$value, colour = .data$threshold)
    }

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = 1), colour = "black",
                          show.legend = FALSE) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = 0.8), colour = "white",
                          show.legend = FALSE) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::scale_y_discrete(limits = rev(levels(data$case))) +
      ggplot2::scale_size_area() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = "Type", y = "Case", colour = "Threshold",
                    size = "Frequency") +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,SimilarityMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "SimilarityMatrix"),
  definition = function(object) {
    # Prepare data
    data <- prepare_spot(object, threshold = NULL, diag = FALSE)
    index_name <- object[["method"]]

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$type, y = .data$case)
    aes_point <- ggplot2::aes(size = .data$value,
                              colour = .data$value)

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = .data$max),
                          colour = "black", show.legend = FALSE) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = .data$max * 0.8),
                          colour = "white", show.legend = FALSE) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::scale_y_discrete(limits = rev(levels(data$case))) +
      ggplot2::scale_size_area() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = "Type", y = "Case", colour = index_name,
                    size = index_name) +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,OccurrenceMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "OccurrenceMatrix"),
  definition = function(object) {
    # Prepare data
    data <- prepare_spot(object, threshold = NULL, diag = FALSE)
    row_names <- unique(data$case)

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$type, y = .data$case)
    aes_point <- ggplot2::aes(size = .data$value * 0.8,
                              colour = .data$value)

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point(ggplot2::aes(size = 1), colour = "black",
                          show.legend = FALSE) +
      ggplot2::geom_point(ggplot2::aes(size = 0.8), colour = "white",
                          show.legend = FALSE) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::scale_x_discrete(position = "top", limits = row_names) +
      ggplot2::scale_y_discrete(limits = rev(row_names)) +
      ggplot2::scale_size_area() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = "Type", y = "Case", colour = "Co-occurrence",
                    size = "Co-occurrence") +
      ggplot2::coord_fixed()
  }
)
