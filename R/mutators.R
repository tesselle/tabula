# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @rdname mutators
#' @aliases labels,DiversityIndex-method
setMethod(
  f = "labels",
  signature = "DiversityIndex",
  definition = function(object) object@labels
)

#' @rdname mutators
#' @aliases labels,RarefactionIndex-method
setMethod(
  f = "labels",
  signature = "RarefactionIndex",
  definition = function(object) object@labels
)

#' @export
#' @rdname mutators
#' @aliases get_method,DiversityIndex-method
setMethod(
  f = "get_method",
  signature = "DiversityIndex",
  definition = function(x) x@method
)

# Setters ======================================================================
