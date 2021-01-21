# PLOT SPOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,CountMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "CountMatrix"),
  definition = function(object, threshold = NULL, ...) {
    freq <- methods::as(object, "AbundanceMatrix")
    plot_spot(freq, threshold = threshold, ...)
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,AbundanceMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "AbundanceMatrix"),
  definition = function(object, threshold = NULL, ...) {
    ## Prepare data
    data <- prepare_spot(object, threshold, ...)

    ## ggplot
    if (is.null(threshold)) {
      aes_point <- ggplot2::aes(size = .data$value)
    } else {
      aes_point <- ggplot2::aes(size = .data$value, colour = .data$colour)
    }

    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(size = 1),
        colour = "black",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(size = 0.8),
        colour = "white",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::labs(
        x = "Type",
        y = "Case",
        colour = "Threshold",
        size = "Frequency"
      ) +
      ggplot2::coord_fixed() +
      ggplot2::scale_size_area() +
      scale_x_matrix(object) +
      scale_y_matrix(object) +
      theme_tabula()
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,SimilarityMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "SimilarityMatrix"),
  definition = function(object, ...) {
    ## Prepare data
    data <- prepare_spot(object, threshold = NULL, diag = FALSE, ...)
    index_name <- arkhe::get_method(object)

    ## ggplot
    aes_point <- ggplot2::aes(size = .data$value, colour = .data$value)

    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(size = .data$max),
        colour = "black",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(size = .data$max * 0.8),
        colour = "white",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::labs(
        x = "Type",
        y = "Case",
        colour = index_name,
        size = index_name
      ) +
      ggplot2::coord_fixed() +
      ggplot2::scale_size_area() +
      scale_x_matrix(object) +
      scale_y_matrix(object) +
      theme_tabula()
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,OccurrenceMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "OccurrenceMatrix"),
  definition = function(object, ...) {
    ## Prepare data
    data <- prepare_spot(object, threshold = NULL, diag = FALSE, ...)
    data$value <- data$value / arkhe::get_totals(object)

    ## ggplot
    aes_point <- ggplot2::aes(size = .data$value * 0.8, colour = .data$value)

    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(size = 1),
        colour = "black",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(size = 0.8),
        colour = "white",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::labs(
        x = "Type",
        y = "Case",
        colour = "Co-occurrence",
        size = "Co-occurrence"
      ) +
      ggplot2::coord_fixed() +
      ggplot2::scale_size_area() +
      scale_x_matrix(object) +
      scale_y_matrix(object) +
      theme_tabula()
  }
)

# Prepare data for Spot plot
# Must return a data.frame
prepare_spot <- function(object, threshold = NULL, diag = TRUE, ...) {
  # Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_long(object, factor = TRUE)

  if (!diag) {
    data <- data[data$column != data$row, ]
  }
  if (nrow(object) == ncol(object)) {
    max_value <- unique(diag(object))
    if (max_value == 0) max_value <- max(data$value)
    data$max <- max_value
  }

  ## Compute threshold, if any
  if (is.function(threshold)) {
    thr <- apply(X = object, MARGIN = 2, FUN = threshold, ...)
    thr <- matrix(thr, nrow = nrow(object), ncol = ncol(object), byrow = TRUE)
    thr <- ifelse(object > thr, "above", "below")
    data$colour <- as.vector(thr)
  } else {
    data$colour <- NA
  }

  ## Spot centers
  data$x <- as.numeric(data$column)
  data$y <- as.numeric(data$row)

  return(data)
}
