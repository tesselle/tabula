# PLOT MATRIX
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_matrix
#' @aliases plot_heatmap,matrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "matrix"),
  definition = function(object) {
    # Prepare data
    data <- prepare_heatmap(object, PVI = FALSE)

    # ggplot
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value) +
      ggplot2::geom_tile() +
      ggplot2::coord_fixed() +
      scale_x_matrix(object, name = "Type") +
      scale_y_matrix(object, name = "Case") +
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
