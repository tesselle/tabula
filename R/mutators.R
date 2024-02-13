# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @method labels DiversityIndex
labels.DiversityIndex <- function(object, ...) object@labels

#' @rdname mutators
#' @aliases labels,DiversityIndex-method
setMethod("labels", "DiversityIndex", labels.DiversityIndex)

#' @export
#' @method labels RarefactionIndex
labels.RarefactionIndex <- function(object, ...) object@labels

#' @rdname mutators
#' @aliases labels,RarefactionIndex-method
setMethod("labels", "RarefactionIndex", labels.RarefactionIndex)

#' @export
#' @rdname mutators
#' @aliases get_method,DiversityIndex-method
setMethod(
  f = "get_method",
  signature = "DiversityIndex",
  definition = function(x) x@method
)

# Setters ======================================================================
