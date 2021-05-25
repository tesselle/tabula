# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame DiversityIndex
#' @export
as.data.frame.DiversityIndex <- function(x, ...) {
  data.frame(
    size = x@size,
    index = x@values,
    row.names = x@names,
    stringsAsFactors = FALSE
  )
}
