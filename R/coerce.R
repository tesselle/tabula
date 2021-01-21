# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame DiversityIndex
#' @export
as.data.frame.DiversityIndex <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    id = rownames(x@data),
    size = x@size,
    index = x@values,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' @method as.data.frame IncrementTest
#' @export
as.data.frame.IncrementTest <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    id = colnames(x@data),
    t = x@statistic,
    p.value = x@p_value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
