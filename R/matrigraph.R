# MATRIGRAPH
#' @include AllGenerics.R
NULL

#' @export
#' @rdname matrigraph
#' @aliases pvi,matrix-method
setMethod(
  f = "pvi",
  signature = signature(object = "matrix"),
  definition = function(object) {
    pvi <- object * 100 / expected(object)
    dimnames(pvi) <- dimnames(object)
    pvi
  }
)

#' @export
#' @rdname matrigraph
#' @aliases pvi,data.frame-method
setMethod(
  f = "pvi",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

#' @export
#' @rdname matrigraph
#' @aliases matrigraph,matrix-method
setMethod(
  f = "matrigraph",
  signature = signature(object = "matrix"),
  definition = function(object, reverse = FALSE, axes = TRUE, ...) {
    plot_matrix(pvi(object), panel = panel_matrigraph, scale = FALSE,
                axes = axes, legend = FALSE, asp = 1, reverse = reverse)
    invisible(object)
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
