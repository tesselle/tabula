#' Deprecated Functions in tabula
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name tabula-deprecated
#' @keywords internal
NULL

#' @export
#' @rdname tabula-deprecated
setGeneric(
  name = "index_sorenson",
  def = function(x, y, ...) standardGeneric("index_sorenson")
)

#' @export
#' @rdname tabula-deprecated
setMethod(
  f = "index_sorenson",
  signature = c(x = "logical", y = "logical"),
  definition = function(x, y) index_sorensen(x, y)
)

#' @export
#' @rdname tabula-deprecated
setMethod(
  f = "index_sorenson",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) index_sorensen(x, y)
)
