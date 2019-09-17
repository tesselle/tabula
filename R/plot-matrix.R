# PLOT MATRIX
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plot_matrix
#' @aliases plot_heatmap,CountMatrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "CountMatrix"),
  definition = function(object, PVI = FALSE, frequency = TRUE) {
    # Prepare data
    data <- prepare_heatmap(object, PVI, frequency)

    # ggplot
    fill <- ifelse(PVI, "PVI", "Frequency")
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, fill = .data[[fill]])
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_tile() +
      ggplot2::scale_x_continuous(
        position = "top",
        expand = c(0, 0),
        limits = range(data$x) + c(-0.5, 0.5),
        breaks = unique(data$x),
        labels = unique(data$type)) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        trans = "reverse",
        breaks = unique(data$y),
        labels = unique(data$case)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = "Type", y = "Case", fill = fill) +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname plot_matrix
#' @aliases plot_heatmap,FrequencyMatrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object) {
    # Prepare data
    data <- prepare_heatmap(object, PVI = FALSE)

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, fill = .data$Frequency)
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_tile() +
      ggplot2::scale_x_continuous(
        position = "top",
        expand = c(0, 0),
        limits = range(data$x) + c(-0.5, 0.5),
        breaks = unique(data$x),
        labels = unique(data$type)) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        trans = "reverse",
        breaks = unique(data$y),
        labels = unique(data$case)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = "Type", y = "Case", fill = "Frequency") +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname plot_matrix
#' @aliases plot_heatmap,IncidenceMatrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object) {
    # Prepare data
    data <- prepare_heatmap(object, PVI = FALSE, frequency = FALSE)

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, fill = .data$Frequency)
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::scale_y_discrete(limits = rev(levels(data$y))) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = "Type", y = "Case", fill = "Value") +
      ggplot2::coord_fixed()
  }
)
