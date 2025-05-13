# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame DiversityIndex
#' @export
as.data.frame.DiversityIndex <- function(x, ...) {
  data.frame(
    size = x@size,
    observed = apply(X = x@data, MARGIN = 1, FUN = observed),
    singleton = apply(X = x@data, MARGIN = 1, FUN = singleton),
    doubleton = apply(X = x@data, MARGIN = 1, FUN = doubleton),
    index = x@.Data,
    row.names = labels(x),
    stringsAsFactors = FALSE
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,DiversityIndex-method
setMethod("as.data.frame", "DiversityIndex", as.data.frame.DiversityIndex)
