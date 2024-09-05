# PLOT HEATMAP
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot_heatmap
#' @aliases plot_heatmap,matrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "matrix"),
  definition = function(object, color = NULL,
                        diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, margin = 1, fixed_ratio = TRUE,
                        axes = TRUE, legend = TRUE, ...) {
    ## Backward compatibility
    col <- list(...)$col
    if (!is.null(col) && is.null(color)) color <- col

    plot_matrix(object, panel = panel_tiles, color = color,
                 diag = diag, upper = upper, lower = lower,
                 freq = freq, margin = margin, drop_zero = FALSE,
                 axes = axes, legend = legend, asp = fixed_ratio)

    invisible(object)
  }
)

#' @export
#' @rdname plot_heatmap
#' @aliases plot_heatmap,data.frame-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "data.frame"),
  definition = function(object, color = NULL,
                        diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, margin = 1, fixed_ratio = TRUE,
                        axes = TRUE, legend = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, color = color,
                         diag = diag, upper = upper, lower = lower,
                         freq = freq, margin = margin,
                         fixed_ratio = fixed_ratio,
                         axes = axes, legend = legend)
  }
)

#' @export
#' @rdname plot_heatmap
#' @aliases plot_heatmap,dist-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "dist"),
  definition = function(object, color = NULL,
                        diag = FALSE, upper = FALSE, lower = !upper,
                        axes = TRUE, legend = TRUE, ...) {
    object <- as.matrix(object)
    methods::callGeneric(object, color = color,
                         diag = diag, upper = upper, lower = lower,
                         axes = axes, legend = legend)
  }
)
