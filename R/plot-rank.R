# PLOT RANK
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plot_line
#' @aliases plot_rank,CountMatrix-method
setMethod(
  f = "plot_rank",
  signature = signature(object = "CountMatrix"),
  definition = function(object, log = NULL, facet = TRUE) {
    freq <- methods::as(object, "FrequencyMatrix")
    plot_rank(freq, log = log, facet = facet)
  }
)

#' @export
#' @rdname plot_line
#' @aliases plot_rank,FrequencyMatrix-method
setMethod(
  f = "plot_rank",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, log = NULL, facet = TRUE) {
    # Prepare data
    data <- prepare_rank(object)
    # Get number of cases
    n <- nrow(object)

    # ggplot
    log_x <- log_y <- NULL
    if (!is.null(log)) {
      if (log == "x" || log == "xy" || log == "yx")
        log_x <- ggplot2::scale_x_log10()
      if (log == "y" || log == "xy" || log == "yx")
        log_y <- ggplot2::scale_y_log10()
    }
    if (facet) {
      facet <- ggplot2::facet_wrap(ggplot2::vars(.data$case), ncol = n)
      aes_plot <- ggplot2::aes(x = .data$rank, y = .data$frequency)
    } else {
      facet <- NULL
      aes_plot <- ggplot2::aes(x = .data$rank, y = .data$frequency,
                               colour = .data$case)
    }
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point() + ggplot2::geom_line() +
      ggplot2::labs(x = "Rank", y = "Frequency",
                    colour = "Assemblage", fill = "Assemblage") +
      log_x + log_y + facet
  }
)
