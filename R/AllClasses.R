# CLASSES DEFINITION AND INITIALIZATION
NULL

# Register S3 classes ==========================================================
setOldClass("dist")

# DiversityIndex ===============================================================
#' Diversity Index
#'
#' An S4 class to represent a diversity measure.
#' @slot names A [`character`] vector giving the sample names.
#' @slot values A [`numeric`] vector giving the diversity index values.
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
#' @author N. Frerebeau
#' @family class
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
    values = "numeric",
    size = "integer",
    simulation = "matrix",
    method = "character"
  ),
  prototype = list(
    names = character(0),
    values = numeric(0),
    size = integer(0),
    simulation = matrix(data = 0, nrow = 0, ncol = 3),
    method = "none"
  )
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

# RefineCA =======================================================================
#' Partial Bootstrap CA
#'
#' An S4 class to store partial bootstrap correspondence analysis results.
#' @slot row_chull A three columns [`numeric`] matrix giving the vertices
#'  coordinates (`x`, `y`) of the samples convex hull and a identifier (`id`) to
#'  link each row to a sample.
#' @slot row_lengths A named [`numeric`] vector giving the convex hull maximum
#'  dimension length of samples.
#' @slot row_keep An [`integer`] vector giving the subscript of the samples to
#'  be kept.
#' @slot column_chull A three columns [`numeric`] matrix giving the vertices
#'  coordinates (`x`, `y`) of the variables convex hull and a identifier (`id`)
#'  to link each row to a variable.
#' @slot column_lengths A [`numeric`] vector giving the convex hull maximum
#'  dimension length of variables.
#' @slot column_keep An [`integer`] vector giving the subscript of the variables
#'  to be kept.
#' @slot cutoff A length-two [`numeric`] vector giving the cutoff value for
#'  samples and variables selection, respectively.
#' @section Subset:
#'  In the code snippets below, `x` is a `RefineCA` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @seealso [`dimensio::CA-class`]
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases RefineCA-class
.RefineCA <- setClass(
  Class = "RefineCA",
  slots = c(
    row_chull = "matrix",
    row_length = "numeric",
    row_keep = "integer",
    column_chull = "matrix",
    column_length = "numeric",
    column_keep = "integer",
    cutoff = "numeric"
  ),
  contains = "CA"
)

# PermutationOrder =============================================================
#' Permutation Order
#'
#' An S4 class to represent a permutation order.
#' @slot rows An [`integer`] vector giving the rows permutation.
#' @slot columns An [`integer`] vector giving the columns permutation.
#' @slot method A [`character`] string indicating the seriation method used.
#' @section Subset:
#'  In the code snippets below, `x` is a `PermutationOrder` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases PermutationOrder-class
.PermutationOrder <- setClass(
  Class = "PermutationOrder",
  slots = c(
    rows = "integer",
    columns = "integer",
    method = "character"
  ),
  prototype = list(
    rows = integer(0),
    columns = integer(0),
    method = "none"
  )
)
