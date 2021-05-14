# CLASSES DEFINITION AND INITIALIZATION
NULL

# Register S3 classes ==========================================================
setOldClass("dist")

# DiversityIndex ===============================================================
#' Diversity Index
#'
#' An S4 class to represent a diversity measure.
#' @slot data A [`numeric`] matrix of count data.
#' @slot values A [`numeric`] vector giving the diversity index values.
#' @slot size A [`integer`] vector giving the sample sizes.
#' @slot simulation A [`numeric`] matrix giving the diversity measures for the
#'  simulated assemblage.
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
#' @slot row_chull A list of length three giving the vertices coordinates
#'  (`x`, `y`) of the samples convex hull and a identifier
#'  (\code{id}) to link each row to a sample.
#' @slot column_chull A list of length three giving the vertices coordinates
#'  (`x`, `y`) of the variables convex hull and a identifier
#'  (\code{id}) to link each row to a variable.
#' @slot row_lengths A list of two named [`numeric`] vectors giving the
#'  convex hull maximum dimension length of samples and variables, respectively.
#' @slot column_lengths A list of two named [`numeric`] vectors giving the
#'  convex hull maximum dimension length of samples and variables, respectively.
#' @slot cutoff A length-two [`numeric`] vector giving the cutoff
#'  value for samples and variables selection, respectively.
#' @slot keep A list of two [`integer`] vectors giving the subscript
#'  of the samples and variables to be kept, respectively.
#' @section Subset:
#'  In the code snippets below, `x` is a `RefineCA` object.
#'  \describe{
#'   \item{\code{x[i, j, drop]}}{Extracts information from a slot selected by
#'   subscript `i` thru `j` (see examples). `i` should be one of
#'   "\code{rows}" or "\code{columns}". Any unambiguous substring can be
#'   given. `j` is a [`numeric`], [`integer`] or
#'   [`character`] vector or empty (missing) or `NULL`.
#'   Numeric values are coerced to [`integer`] as by
#'   [as.integer()] (and hence truncated towards zero). Character
#'   vectors will be matched to the name of the elements. An empty index
#'   (a comma separated blank) indicates that all entries in that dimension are
#'   selected.}
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector. Returns the corresponding slot
#'   values.}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases RefineCA-class
.RefineCA <- setClass(
  Class = "RefineCA",
  slots = c(
    row_chull = "matrix",
    row_length = "numeric",
    row_keep = "numeric",
    column_chull = "matrix",
    column_length = "numeric",
    column_keep = "numeric",
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
