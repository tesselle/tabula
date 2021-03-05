# PLOT SPOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,matrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "matrix"),
  definition = function(object, threshold = NULL,
                        diag = TRUE, upper = TRUE, ...) {
    ## Prepare data
    data <- object / rowSums(object)
    data <- prepare_spot(data, threshold, diag = diag, upper = upper, ...)

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
      ggplot2::coord_fixed() +
      ggplot2::scale_size_area() +
      scale_x_matrix(object, name = "Type") +
      scale_y_matrix(object, name = "Case") +
      theme_tabula()
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,dist-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "dist"),
  definition = function(object, diag = FALSE, upper = FALSE, ...) {
    ## Prepare data
    index_name <- attr(object, "method")
    object <- as.matrix(object)
    data <- prepare_spot(object, threshold = NULL, diag = diag,
                         upper = upper, ...)

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
      ggplot2::coord_fixed() +
      ggplot2::scale_size_area() +
      scale_x_matrix(object, name = "Type") +
      scale_y_matrix(object, name = "Case") +
      theme_tabula()
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,OccurrenceMatrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "OccurrenceMatrix"),
  definition = function(object, diag = FALSE, upper = FALSE, ...) {
    ## Prepare data
    data <- prepare_spot(object, threshold = NULL, diag = diag,
                         upper = upper, ...)
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
      ggplot2::coord_fixed() +
      ggplot2::scale_size_area() +
      scale_x_matrix(object, name = "Type") +
      scale_y_matrix(object, name = "Type") +
      theme_tabula()
  }
)

# Prepare data for Spot plot
# Must return a data.frame
prepare_spot <- function(object, threshold = NULL, diag = TRUE,
                         upper = TRUE, ...) {
  max_value <- max(diag(object))
  max_value <- ifelse(max_value == 0, max(object), max_value)

  # Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_long(object, factor = TRUE)
  data$max <- max_value

  if (!upper) {
    data <- data[!upper.tri(object), ]
  }
  if (!diag) {
    data <- data[data$row != data$column, ]
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
