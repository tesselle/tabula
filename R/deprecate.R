# DEPRECATED METHODS
NULL

#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,CountMatrix-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "CountMatrix"),
  definition = function(object, margin = c(1, 2), ...) {
    .Deprecated(new = "seriate_average")
    seriate_average2(object = object, margin = margin, ...)
  }
)

#' @export
#' @rdname seriation
#' @aliases seriate_correspondence,IncidenceMatrix-method
setMethod(
  f = "seriate_correspondence",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, margin = c(1, 2), ...) {
    .Deprecated(new = "seriate_average")
    seriate_average2(object = object, margin = margin, ...)
  }
)

#' @export
#' @rdname deprecate
#' @aliases seriate_reciprocal,CountMatrix-method
setMethod(
  f = "seriate_reciprocal",
  signature = signature(object = "CountMatrix"),
  definition = function(object, EPPM = FALSE, margin = c(1, 2), stop = 100) {
    .Deprecated(new = "seriate_rank")
    seriate_rank2(object, margin = margin, stop = stop, EPPM = EPPM)
  }
)

#' @export
#' @rdname deprecate
#' @aliases seriate_reciprocal,IncidenceMatrix-method
setMethod(
  f = "seriate_reciprocal",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, margin = c(1, 2), stop = 100) {
    .Deprecated(new = "seriate_rank")
    seriate_rank2(object, margin = margin, stop = stop, EPPM = FALSE)
  }
)
