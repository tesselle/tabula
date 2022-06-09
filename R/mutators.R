# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutator
#' @aliases get_method,DiversityIndex-method
setMethod(
  f = "get_method",
  signature = "DiversityIndex",
  definition = function(x) x@method
)

#' @export
#' @rdname seriation
#' @aliases get_order,PermutationOrder-method
setMethod("get_order", "PermutationOrder", function(x) {
  list(rows = x@rows, columns = x@columns)
})

# Setters ======================================================================
