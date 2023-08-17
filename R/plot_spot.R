# PLOT SPOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,matrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "matrix"),
  definition = function(object, type = c("ring", "plain"),
                        col = grDevices::hcl.colors(12, "YlOrBr", rev = TRUE),
                        diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, margin = 1,
                        axes = TRUE, legend = TRUE, ...) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    plot_matrix(object, panel = panel_spot, col = col,
                 diag = diag, upper = upper, lower = lower,
                 freq = freq, margin = margin, drop_zero = TRUE,
                 axes = axes, legend = legend, type = type)

    invisible(object)
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,data.frame-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "data.frame"),
  definition = function(object, type = c("ring", "plain"),
                        col = grDevices::hcl.colors(12, "YlOrBr", rev = TRUE),
                        diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, margin = 1,
                        axes = TRUE, legend = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, type = type, col = col,
                         diag = diag, upper = upper, lower = lower,
                         freq = freq, margin = margin,
                         axes = axes, legend = legend, ...)
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,dist-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "dist"),
  definition = function(object, type = c("ring", "plain"),
                        col = grDevices::hcl.colors(12, "YlOrBr", rev = TRUE),
                        diag = FALSE, upper = FALSE, lower = !upper,
                        axes = TRUE, legend = TRUE, ...) {
    # index_name <- attr(object, "method")
    object <- as.matrix(object)
    methods::callGeneric(object, type = type, col = col,
                         diag = diag, upper = upper, lower = lower,
                         axes = axes, legend = legend)
  }
)
