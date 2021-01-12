#' @include AllClasses.R AllGenerics.R
NULL

# #' @export
# #' @rdname seriation
# #' @aliases permute,CountMatrix,CA-method
# setMethod(
#   f = "permute",
#   signature = signature(object = "CountMatrix", order = "CA"),
#   definition = function(object, order) {
#     # Rearrange matrix
#     object[order[["rows"]], order[["columns"]]]
#   }
# )

#' @export
#' @rdname seriation
#' @aliases permute,CountMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "CountMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange matrix
    object[order[["rows"]], order[["columns"]]]
  }
)

#' @export
#' @rdname seriation
#' @aliases permute,IncidenceMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "IncidenceMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Rearrange matrix
    object[order[["rows"]], order[["columns"]]]
  }
)
