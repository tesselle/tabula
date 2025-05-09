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

#' @export
#' @rdname tabula-deprecated
setGeneric(
  name = "resample",
  def = function(object, ...) standardGeneric("resample")
)

#' @export
#' @rdname tabula-deprecated
setMethod(
  f = "resample",
  signature = c(object = "numeric"),
  definition = function(object, do, n, size = sum(object), ..., f = NULL) {
    .Deprecated(new = "arkhe::resample_multinomial()", old = "resample()")
    ## Validation
    arkhe::assert_count(object)

    prob <- object / sum(object)
    replicates <- stats::rmultinom(n, size = size, prob = prob)
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    if (is.function(f)) values <- f(values)
    values
  }
)
