# MUTATORS
#' @include AllClasses.R
NULL

# ====================================================================== Getters
#' @export
#' @rdname seriation
#' @aliases get_order,PermutationOrder-method
setMethod("get_order", "PermutationOrder", function(object) {
  list(rows = object@rows, columns = object@columns)
})

# ====================================================================== Setters
