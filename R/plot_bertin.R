# PLOT BERTIN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot_bertin
#' @aliases plot_bertin,matrix-method
setMethod(
  f = "plot_bertin",
  signature = signature(object = "matrix"),
  definition = function(object, threshold = NULL, freq = FALSE, margin = 1,
                        color = c("white", "black"), flip = TRUE,
                        axes = TRUE, ...) {
    ## Conditional proportions
    if (freq) object <- prop.table(object, margin = margin)

    ## Compute threshold for each variable
    if (!freq && is.function(threshold)) {
      thr <- apply(X = object, MARGIN = 2, FUN = threshold)
      thr <- matrix(thr, nrow = nrow(object), ncol = ncol(object), byrow = TRUE)
      thr <- ifelse(object > thr, color[length(color)], color[1L])
      color <- as.vector(t(thr))
    }

    ## /!\ Bertin plot flips x and y axis /!\
    data <- if (flip) t(object) else object

    plot_matrix(data, panel = panel_bertin, color = color,
                axes = axes, legend = FALSE, asp = NA)

    ## Legend
    # TODO

    invisible(object)
  }
)

#' @export
#' @rdname plot_bertin
#' @aliases plot_bertin,data.frame-method
setMethod(
  f = "plot_bertin",
  signature = signature(object = "data.frame"),
  definition = function(object, threshold = NULL, freq = FALSE, margin = 1,
                        color = c("white", "black"), flip = TRUE,
                        axes = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, threshold = threshold,
                         freq = freq, margin = margin,
                         color = color, flip = flip, axes = axes)
  }
)
