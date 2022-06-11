# PLOT HEATMAP
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_heatmap
#' @aliases plot_heatmap,matrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "matrix"),
  definition = function(object, diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE) {
    ## Prepare data
    data <- prepare_heatmap(object, diag = diag, upper = upper, lower = lower,
                            freq = freq, PVI = FALSE)

    ## ggplot
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value) +
      ggplot2::geom_tile() +
      ggplot2::coord_fixed() +
      scale_x_matrix(object, name = "Type") +
      scale_y_matrix(object, name = "Case") +
      theme_tabula()
  }
)

#' @export
#' @rdname plot_heatmap
#' @aliases plot_heatmap,data.frame-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "data.frame"),
  definition = function(object, diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, diag = diag, upper = upper, lower = lower,
                         freq = freq)
  }
)

#' @export
#' @rdname plot_heatmap
#' @aliases plot_heatmap,dist-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "dist"),
  definition = function(object, diag = FALSE, upper = FALSE, lower = !upper) {
    object <- as.matrix(object)
    methods::callGeneric(object, diag = diag, upper = upper, lower = lower)
  }
)

## Prepare data for heatmap plot
## Must return a data.frame
prepare_heatmap <- function(object, diag = TRUE, upper = TRUE, lower = TRUE,
                            freq = FALSE, threshold = NULL, drop_zero = FALSE,
                            PVI = FALSE, ...) {
  ## Validation
  if (!arkhe::is_symmetric(object)) {
    diag <- TRUE
    upper <- TRUE
    lower <- TRUE
  }

  ## Relative frequencies
  object <- if (freq) object / rowSums(object) else object

  ## /!\ PVI computation needs count data
  data <- if (PVI) pvi(object) else object

  ## Build long table from data
  data <- arkhe::as_long(data, factor = TRUE)
  # max_value <- max(diag(object))
  # max_value <- ifelse(max_value == 0, max(object), max_value)
  data$max <- max(object)

  ## Compute threshold, if any
  if (is.function(threshold)) {
    thr <- apply(X = object, MARGIN = 2, FUN = threshold, ...)
    thr <- matrix(thr, nrow = nrow(object), ncol = ncol(object), byrow = TRUE)
    thr <- ifelse(object > thr, "above", "below")
    data$threshold <- as.vector(thr)
  } else {
    data$threshold <- NA
  }

  if (!upper) {
    data <- data[!upper.tri(object), ]
  }
  if (!lower) {
    data <- data[!lower.tri(object), ]
  }
  if (!diag) {
    data <- data[data$row != data$column, ]
  }
  if (drop_zero) {
    data <- data[data$value != 0, ]
  }

  ## Tile centers
  data$x <- as.numeric(data$column)
  data$y <- as.numeric(data$row)

  return(data)
}
