# PLOT HEATMAP
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot_heatmap
#' @aliases plot_heatmap,matrix-method
setMethod(
  f = "plot_heatmap",
  signature = signature(object = "matrix"),
  definition = function(object, col = grDevices::hcl.colors(12, "YlOrBr", rev = TRUE),
                        diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, margin = 1, fixed_ratio = TRUE,
                        axes = TRUE, legend = TRUE, ...) {

    plot_matrix(object, panel = panel_tiles, col = col,
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
  definition = function(object, col = grDevices::hcl.colors(12, "YlOrBr", rev = TRUE),
                        diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, margin = 1, fixed_ratio = TRUE,
                        axes = TRUE, legend = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, col = col,
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
  definition = function(object, col = grDevices::hcl.colors(12, "YlOrBr", rev = TRUE),
                        diag = FALSE, upper = FALSE, lower = !upper,
                        axes = TRUE, legend = TRUE, ...) {
    object <- as.matrix(object)
    methods::callGeneric(object, col = col,
                         diag = diag, upper = upper, lower = lower,
                         axes = axes, legend = legend)
  }
)
