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
    eppm <- round((object - expected(object)) * 100 / rowSums(object))
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
  definition = function(object, weights = FALSE) {
    plot_ford(object, weights = weights, EPPM = TRUE)
    invisible(object)
  }
)

#' @export
#' @rdname seriograph
#' @aliases seriograph,data.frame-method
setMethod(
  f = "seriograph",
  signature = signature(object = "data.frame"),
  definition = function(object, weights = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, weights = weights)
  }
)
