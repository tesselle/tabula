#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases permute,CountMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "CountMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Validation
    if (arkhe::get_id(object) != arkhe::get_id(order))
      stop("`object` and `order` do not match.")

    # Rearrange matrix
    new_matrix <- object[order[["rows"]], order[["columns"]]]
    # New CountMatrix object
    arkhe::as_count(new_matrix)
  }
)

#' @export
#' @rdname seriation
#' @aliases permute,IncidenceMatrix,PermutationOrder-method
setMethod(
  f = "permute",
  signature = signature(object = "IncidenceMatrix", order = "PermutationOrder"),
  definition = function(object, order) {
    # Validation
    if (arkhe::get_id(object) != arkhe::get_id(order))
      stop("`object` and `order` do not match.")

    # Rearrange matrix
    new_matrix <- object[order[["rows"]], order[["columns"]]]
    # New CountMatrix object
    arkhe::as_incidence(new_matrix)
  }
)
