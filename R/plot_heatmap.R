# PLOT MATRIX
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @method autoplot matrix
autoplot.matrix <- function(object, ..., diag = TRUE,
                            upper = TRUE, lower = TRUE) {
  ## Prepare data
  data <- prepare_heatmap(object, diag = diag, upper = upper, lower = lower,
                          PVI = FALSE)

  ## ggplot
  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value) +
    ggplot2::geom_tile() +
    ggplot2::coord_fixed() +
    scale_x_matrix(object, name = "Type") +
    scale_y_matrix(object, name = "Case") +
    theme_tabula()
}

#' @export
#' @rdname plot_matrix
#' @aliases autoplot,matrix-method
setMethod("autoplot", "matrix", autoplot.matrix)

#' @export
#' @rdname plot_matrix
#' @aliases autoplot,dist-method
setMethod(
  f = "autoplot",
  signature = signature(object = "dist"),
  definition = function(object, ..., diag = FALSE,
                        upper = FALSE, lower = !upper) {
    object <- as.matrix(object)
    methods::callGeneric(object, ..., diag = diag, upper = upper, lower = lower)
  }
)

#' @export
#' @rdname plot_matrix
#' @aliases autoplot,OccurrenceMatrix-method
setMethod(
  f = "autoplot",
  signature = signature(object = "OccurrenceMatrix"),
  definition = function(object, ..., diag = FALSE,
                        upper = FALSE, lower = !upper) {
    methods::callNextMethod(object, ..., diag = diag,
                            upper = upper, lower = lower)
  }
)

#' @export
#' @method plot matrix
plot.matrix <- function(x, diag = TRUE, upper = TRUE, lower = TRUE, ...) {
  gg <- autoplot(object = x, diag = diag, upper = upper, lower = lower)
  print(gg)
  invisible(x)
}

#' @export
#' @method plot dist
plot.dist <- function(x, diag = FALSE, upper = FALSE, lower = !upper, ...) {
  gg <- autoplot(object = x, diag = diag, upper = upper, lower = lower)
  print(gg)
  invisible(x)
}

#' @export
#' @method plot OccurrenceMatrix
plot.OccurrenceMatrix <- function(x, diag = FALSE,
                                  upper = FALSE, lower = !upper, ...) {
  gg <- autoplot(object = x, diag = diag, upper = upper, lower = lower)
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_matrix
#' @aliases plot,matrix,missing-method
setMethod("plot", c(x = "matrix", y = "missing"), plot.matrix)

#' @export
#' @rdname plot_matrix
#' @aliases plot,dist,missing-method
setMethod("plot", c(x = "dist", y = "missing"), plot.dist)

#' @export
#' @rdname plot_matrix
#' @aliases plot,OccurrenceMatrix,missing-method
setMethod("plot", c(x = "OccurrenceMatrix", y = "missing"),
          plot.OccurrenceMatrix)

## Prepare data for heatmap plot
## Must return a data.frame
prepare_heatmap <- function(object, diag = TRUE, upper = TRUE, lower = TRUE,
                            threshold = NULL, drop_zero = FALSE,
                            PVI = FALSE, ...) {
  ## Validation
  if (!arkhe::is_symmetric(object)) {
    diag <- TRUE
    upper <- TRUE
    lower <- TRUE
  }

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
