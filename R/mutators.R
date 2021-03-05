# MUTATORS
#' @include AllClasses.R
NULL

# ====================================================================== Getters
#' @export
#' @rdname mutator
#' @aliases get_index,HeterogeneityIndex-method
setMethod(
  f = "get_index",
  signature = "HeterogeneityIndex",
  definition = function(x) switch_heterogeneity(x@method)
)

#' @export
#' @rdname mutator
#' @aliases get_index,EvennessIndex-method
setMethod(
  f = "get_index",
  signature = "EvennessIndex",
  definition = function(x) switch_evenness(x@method)
)

#' @export
#' @rdname mutator
#' @aliases get_index,RichnessIndex-method
setMethod(
  f = "get_index",
  signature = "RichnessIndex",
  definition = function(x) switch_richness(x@method)
)

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

# ====================================================================== Setters
