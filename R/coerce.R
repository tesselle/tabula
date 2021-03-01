# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame DiversityIndex
#' @export
as.data.frame.DiversityIndex <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    size = x@size,
    index = x@values,
    row.names = x@names,
    stringsAsFactors = FALSE
  )
}

#' @method as.data.frame IncrementTest
#' @export
as.data.frame.IncrementTest <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    t = x@statistic,
    p.value = x@p_value,
    row.names = colnames(x@data),
    stringsAsFactors = FALSE
  )
}

#' @method as.data.frame DateMCD
#' @export
as.data.frame.DateMCD <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    date = x@mcd_values,
    error = x@mcd_errors,
    row.names = rownames(x@data),
    stringsAsFactors = FALSE
  )
}
