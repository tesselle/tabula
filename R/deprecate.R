# DEPRECATED METHODS
NULL

#' @export
#' @rdname deprecate
#' @aliases seriate_reciprocal,CountMatrix-method
setMethod(
  f = "seriate_reciprocal",
  signature = signature(object = "CountMatrix"),
  definition = function(object, EPPM = FALSE, margin = c(1, 2), stop = 100) {
    .Deprecated(new = "seriate_rank")
    seriate_rank(object, margin = margin, stop = stop, EPPM = EPPM)
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
    seriate_rank(object, margin = margin, stop = stop)
  }
)

#' @export
#' @rdname deprecate
#' @aliases refine_seriation,CountMatrix-method
setMethod(
  f = "refine_seriation",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2), ...) {
    .Deprecated(new = "refine_seriation")
    object <- arkhe::ca(object)
    refine_ca(object, cutoff, n, axes, ...)
  }
)
