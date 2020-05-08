# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
setAs(
  from = "DiversityIndex",
  to = "data.frame",
  def = function(from) {
    data.frame(
      size = from@size,
      index = from@index,
      stringsAsFactors = FALSE
    )
  }
)
