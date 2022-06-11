# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "DiversityIndex",
  definition = function(object) {
    methods::callGeneric(object@.Data)
  }
)
setMethod(
  f = "show",
  signature = "RarefactionIndex",
  definition = function(object) {
    methods::callGeneric(object@.Data)
  }
)
