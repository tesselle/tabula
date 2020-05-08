# PLOT FORD
#' @include AllClasses.R AllGenerics.R
NULL

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

# Prepare data for Ford plot
# Must return a data.frame
prepare_ford <- function(object, EPPM = FALSE) {
  # Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_abundance(object)
  data <- arkhe::as_long(data, as_factor = TRUE)
  data$case <- factor(data$case, levels = rev(unique(data$case)))

  if (EPPM) {
    # Build long table from threshold
    threshold <- calculate_eppm(object)
    threshold <- wide2long(threshold, value = "EPPM")

    # Join data and threshold
    data <- merge(data, threshold, by = c("case", "type"), all = TRUE)
    data$data <- data$data - data$EPPM
    data_stacked <- utils::stack(data[, !(names(data) %in% c("case", "type"))])
    data <- cbind.data.frame(data$case, data$type, data_stacked)
    colnames(data) <- c("case", "type", "data", "threshold")
  }

  k <- nrow(data)
  z <- c(rep(1, k), rep(-1, k)) / 2
  data <- rbind.data.frame(data, data)
  data$data <- data$data * z

  return(data)
}
