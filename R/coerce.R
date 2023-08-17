# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame DiversityIndex
#' @export
as.data.frame.DiversityIndex <- function(x, ...) {
  data.frame(
    size = x@size,
    index = x@.Data,
    row.names = labels(x),
    stringsAsFactors = FALSE
  )
}
