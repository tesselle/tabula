# CLASSES DEFINITION AND INITIALIZATION
NULL

# Register S3 classes ==========================================================
setOldClass("dist")

# DiversityIndex ===============================================================
#' Diversity Index
#'
#' An S4 class to represent a diversity measure.
#' @slot .Data A [`numeric`] vector of diversity measures.
#' @slot labels A [`character`] vector giving the sample names.
#' @slot size An [`integer`] vector giving the sample sizes.
#' @slot data A [`numeric`] matrix of count data.
#' @slot method A [`character`] string specifying the method used.
#' @slot simulation A four columns [`numeric`] matrix giving the diversity
#'  measures for the simulated assemblage (sample `size`, `mean` estimate,
#'  `lower` and `upper` boundaries of the confidence interval).
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
#' @aliases DiversityIndex-class
#' @keywords internal
.DiversityIndex <- setClass(
  Class = "DiversityIndex",
  slots = c(
    labels = "character",
    size = "integer",
    data = "matrix",
    method = "character",
    simulation = "matrix"
  ),
  contains = "numeric"
)

#' Heterogeneity Index
#'
#' An S4 class to represent an heterogeneity measure.
#' @note
#'  This class inherits from [DiversityIndex-class].
#' @seealso [heterogeneity()]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases HeterogeneityIndex-class
#' @keywords internal
.HeterogeneityIndex <- setClass(
  Class = "HeterogeneityIndex",
  contains = "DiversityIndex"
)

#' Evenness Index
#'
#' An S4 class to represent an evenness measure.
#' @note
#'  This class inherits from [DiversityIndex-class].
#' @seealso [evenness()]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases EvennessIndex-class
#' @keywords internal
.EvennessIndex <- setClass(
  Class = "EvennessIndex",
  contains = "DiversityIndex"
)

#' Richness Index
#'
#' An S4 class to represent a richness measure.
#' @note
#'  This class inherits from [DiversityIndex-class].
#' @seealso [richness()]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases RichnessIndex-class
#' @keywords internal
.RichnessIndex <- setClass(
  Class = "RichnessIndex",
  contains = "DiversityIndex"
)

#' Composition Index
#'
#' An S4 class to represent an composition measure.
#' @note
#'  This class inherits from [DiversityIndex-class].
#' @seealso [composition()]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CompositionIndex-class
#' @keywords internal
.CompositionIndex <- setClass(
  Class = "CompositionIndex",
  contains = "DiversityIndex"
)

#' Rarefaction Index
#'
#' An S4 class to represent an rarefaction measure.
#' @slot .Data A [`numeric`] matrix of count data (expected taxa per fixed
#'  number of individuals).
#' @slot labels A [`character`] vector giving the sample names.
#' @slot size An [`integer`] vector giving the sample sizes.
#' @slot method A [`character`] string specifying the method used.
#' @note
#'  This class inherits from base [`matrix`].
#' @seealso [rarefaction()]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases RarefactionIndex-class
#' @keywords internal
.RarefactionIndex <- setClass(
  Class = "RarefactionIndex",
  slots = c(
    labels = "character",
    size = "integer",
    method = "character"
  ),
  contains = "matrix"
)
