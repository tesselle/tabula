# CLASSES DEFINITION AND INITIALIZATION
NULL

# Register S3 classes ==========================================================
setOldClass("dist")

# DiversityIndex ===============================================================
#' Diversity Index
#'
#' An S4 class to represent a diversity measure.
#' @slot names A [`character`] vector giving the sample names.
#' @slot size A [`integer`] vector giving the sample sizes.
#' @slot simulation A four columns [`numeric`] matrix giving the diversity
#'  measures for the simulated assemblage (sample `size`, `mean` estimate,
#'  `lower` and `upper` boundaries of the confidence interval).
#' @slot method A [`character`] string specifying the method used.
#' @section Subset:
#'  In the code snippets below, `x` is a `DiversityIndex` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @section Coerce:
#'  In the code snippets below, `x` is an `DiversityIndex` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from base [`numeric`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @name DiversityIndex
#' @rdname DiversityIndex
NULL

#' @rdname DiversityIndex
#' @aliases DiversityIndex-class
.DiversityIndex <- setClass(
  Class = "DiversityIndex",
  slots = c(
    names = "character",
    size = "integer",
    data = "matrix",
    simulation = "matrix",
    method = "character"
  ),
  contains = "numeric"
)
#' @rdname DiversityIndex
#' @aliases HeterogeneityIndex-class
.HeterogeneityIndex <- setClass(
  Class = "HeterogeneityIndex",
  contains = "DiversityIndex"
)
#' @rdname DiversityIndex
#' @aliases EvennessIndex-class
.EvennessIndex <- setClass(
  Class = "EvennessIndex",
  contains = "DiversityIndex"
)
#' @rdname DiversityIndex
#' @aliases RichnessIndex-class
.RichnessIndex <- setClass(
  Class = "RichnessIndex",
  contains = "DiversityIndex"
)
#' @rdname DiversityIndex
#' @aliases CompositionIndex-class
.CompositionIndex <- setClass(
  Class = "CompositionIndex",
  contains = "DiversityIndex"
)

#' @rdname DiversityIndex
#' @aliases RarefactionIndex-class
.RarefactionIndex <- setClass(
  Class = "RarefactionIndex",
  slots = c(
    names = "character",
    size = "integer",
    method = "character"
  ),
  contains = "matrix"
)
