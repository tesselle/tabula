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
                        col = c("white", "black"), flip = TRUE,
                        axes = TRUE, ...) {
    ## Conditional proportions
    if (freq) object <- prop.table(object, margin = margin)

    ## Compute threshold for each variable
    if (!freq && is.function(threshold)) {
      thr <- apply(X = object, MARGIN = 2, FUN = threshold)
      thr <- matrix(thr, nrow = nrow(object), ncol = ncol(object), byrow = TRUE)
      thr <- ifelse(object > thr, col[length(col)], col[1L])
      col <- as.vector(t(thr))
    }

    ## /!\ Bertin plot flips x and y axis /!\
    data <- if (flip) t(object) else object

    plot_matrix(data, panel = panel_bertin, col = col,
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
                        col = c("white", "black"), flip = TRUE,
                        axes = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, threshold = threshold,
                         freq = freq, margin = margin,
                         col = col, flip = flip, axes = axes)
  }
)
