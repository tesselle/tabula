# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
setAs(
  from = "DiversityIndex",
  to = "data.frame",
  def = function(from) {
    x <- data.frame(
      size = from@size,
      index = from@index,
      stringsAsFactors = FALSE
    )
    attr(x, "id") <- from@id
    x
  }
)
