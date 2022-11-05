# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases get_method,DiversityIndex-method
setMethod(
  f = "get_method",
  signature = "DiversityIndex",
  definition = function(x) x@method
)

# Setters ======================================================================
