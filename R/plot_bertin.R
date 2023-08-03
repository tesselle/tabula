# PLOT BERTIN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot_bertin
#' @aliases plot_bertin,matrix-method
setMethod(
  f = "plot_bertin",
  signature = signature(object = "matrix"),
  definition = function(object, threshold = NULL, scale = NULL,
                        col = c("white", "black"), axes = TRUE, ...) {
    ## Scale variables
    if (is.function(scale)) {
      object <- apply(X = object, MARGIN = 2, FUN = scale)
    }

    ## Compute threshold for each variable
    if (is.function(threshold)) {
      thr <- apply(X = object, MARGIN = 2, FUN = threshold)
      thr <- matrix(thr, nrow = nrow(object), ncol = ncol(object), byrow = TRUE)
      thr <- ifelse(object > thr, "black", "white")
      col <- as.vector(t(thr))
    }

    ## /!\ Bertin plot flips x and y axis /!\
    object <- t(object)

    .plot_matrix(object, panel = panel_bertin, col = col,
                 axes = axes, legend = FALSE, asp = NA)

    ## Legend
    # TODO

    invisible(t(object))
  }
)

#' @export
#' @rdname plot_bertin
#' @aliases plot_bertin,data.frame-method
setMethod(
  f = "plot_bertin",
  signature = signature(object = "data.frame"),
  definition = function(object, threshold = NULL, scale = NULL,
                        col = c("white", "black"), axes = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, threshold = threshold, scale = scale,
                         col = col, axes = axes)
  }
)
