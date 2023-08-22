# SERIOGRAPH
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriograph
#' @aliases eppm,matrix-method
setMethod(
  f = "eppm",
  signature = signature(object = "matrix"),
  definition = function(object) {
    eppm <- (object - expected(object)) * 100 / rowSums(object)
    eppm[eppm < 0] <- 0
    dimnames(eppm) <- dimnames(object)
    eppm
  }
)

#' @export
#' @rdname seriograph
#' @aliases eppm,data.frame-method
setMethod(
  f = "eppm",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

#' @export
#' @rdname seriograph
#' @aliases seriograph,matrix-method
setMethod(
  f = "seriograph",
  signature = signature(object = "matrix"),
  definition = function(object, weights = FALSE,
                        fill = "darkgrey", border = NA,
                        axes = TRUE, ...) {
    plot_ford(object, weights = weights, EPPM = TRUE,
              fill = fill, border = border, axes = axes)
    invisible(object)
  }
)

#' @export
#' @rdname seriograph
#' @aliases seriograph,data.frame-method
setMethod(
  f = "seriograph",
  signature = signature(object = "data.frame"),
  definition = function(object, weights = FALSE,
                        fill = "darkgrey", border = NA,
                        axes = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, weights = weights,
                         fill = fill, border = border, axes = axes)
  }
)
