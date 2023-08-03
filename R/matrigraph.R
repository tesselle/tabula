# MATRIGRAPH
#' @include AllGenerics.R
NULL

#' @export
#' @rdname matrigraph
#' @aliases matrigraph,matrix-method
setMethod(
  f = "matrigraph",
  signature = signature(object = "matrix"),
  definition = function(object, reverse = FALSE, axes = TRUE, ...) {

    ## PVI
    object <- pvi(object)

    .plot_matrix(object, panel = panel_matrigraph, scale = FALSE,
                 axes = axes, legend = FALSE, asp = 1, reverse = reverse)

    ## Legend
    # TODO

    invisible(t(object))
  }
)

#' @export
#' @rdname matrigraph
#' @aliases matrigraph,data.frame-method
setMethod(
  f = "matrigraph",
  signature = signature(object = "data.frame"),
  definition = function(object, reverse = FALSE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, reverse = reverse, ...)
  }
)
