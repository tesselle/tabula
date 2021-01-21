# PLOT MATRIX
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_matrix
#' @aliases plot_heatmap,CountMatrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "CountMatrix"),
  definition = function(object, PVI = FALSE) {
    ## Prepare data
    data <- prepare_heatmap(object, PVI = PVI)

    ## ggplot
    fill_lab <- ifelse(PVI, "PVI", "Frequency")

    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value) +
      ggplot2::geom_tile() +
      ggplot2::labs(x = "Type", y = "Case", fill = fill_lab) +
      ggplot2::coord_fixed() +
      scale_x_matrix(object) +
      scale_y_matrix(object) +
      theme_tabula()
  }
)

#' @export
#' @rdname plot_matrix
#' @aliases plot_heatmap,AbundanceMatrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "AbundanceMatrix"),
  definition = function(object) {
    # Prepare data
    data <- prepare_heatmap(object, PVI = FALSE)

    # ggplot
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value) +
      ggplot2::geom_tile() +
      ggplot2::labs(x = "Type", y = "Case", fill = "Frequency") +
      ggplot2::coord_fixed() +
      scale_x_matrix(object) +
      scale_y_matrix(object) +
      theme_tabula()
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
    data <- prepare_heatmap(object, PVI = FALSE)

    # ggplot
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::labs(x = "Type", y = "Case", fill = "Value") +
      ggplot2::coord_fixed() +
      scale_x_matrix(object) +
      scale_y_matrix(object) +
      theme_tabula()
  }
)

## Prepare data for heatmap plot
## Must return a data.frame
prepare_heatmap <- function(object, PVI = FALSE) {
  ## /!\ PVI computation needs count data
  data <- if (PVI) pvi(object) else object

  ## Build long table from data
  data <- arkhe::as_long(data, factor = TRUE)
  ## Tile centers
  data$x = as.numeric(data$column)
  data$y = as.numeric(data$row)

  return(data)
}
