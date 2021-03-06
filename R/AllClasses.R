# CLASSES DEFINITION AND INITIALIZATION
NULL

# Register S3 classes ==========================================================
setOldClass("dist")

# DiversityIndex ===============================================================
#' Diversity Index
#'
#' An S4 class to represent a diversity measure.
#' @slot data A \code{\link{numeric}} matrix of count data.
#' @slot values A \code{\link{numeric}} vector giving the diversity index values.
#' @slot size A \code{\link{integer}} vector giving the sample sizes.
#' @slot simulation A \code{\link{numeric}} matrix giving the diversity
#'  measures for the simulated assemblage.
#' @slot method A \code{\link{character}} string specifying the method used.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{DiversityIndex} object.
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector. Returns the corresponding slot values.}
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

# IncrementTest ================================================================
#' Frequency Increment Test
#'
#' An S4 class to represent a Frequency Increment Test results.
#' @slot data A \code{\link{numeric}} matrix of count data.
#' @slot dates A \code{\link{numeric}} vector of dates.
#' @slot statistic A \code{\link{numeric}} vector giving the values of the
#'  t-statistic.
#' @slot parameter An \code{\link{integer}} giving the degrees of freedom for
#'  the t-statistic.
#' @slot p_value A \code{\link{numeric}} vector giving the the p-value for the
#'  test.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{IncrementTest} object.
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector. Returns the corresponding slot values.}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases IncrementTest-class
.IncrementTest <- setClass(
  Class = "IncrementTest",
  slots = c(
    data = "matrix",
    dates = "numeric",
    statistic = "numeric",
    parameter = "integer",
    p_value = "numeric"
  )
)

# DateMCD ======================================================================
#' Mean Ceramic Date
#'
#' S4 classes to store the Mean Ceramic Date of archaeological assemblages.
#' @slot data A \code{\link{numeric}} matrix of count data.
#' @slot dates A \code{\link{numeric}} vector of dates.
#' @slot mcd_values A \code{\link{numeric}} vector of errors.
#' @slot mcd_errors A \code{\link{numeric}} vector of errors.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{DateMCD} object.
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector. Returns the corresponding slot values.}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases DateMCD-class
.DateMCD <- setClass(
  Class = "DateMCD",
  slots = c(
    data = "matrix",
    dates = "numeric",
    mcd_values = "numeric",
    mcd_errors = "numeric"
  )
)

# DateModel ====================================================================
#' Date Model
#'
#' S4 classes to store the event and accumulation times of archaeological
#'  assemblages.
#' @slot data A \code{\link{numeric}} matrix of count data.
#' @slot dates A \code{\link{numeric}} vector of dates.
#' @slot model A \code{\link[stats:lm]{multiple linear model}}: the Gaussian
#'  multiple linear regression model fitted for event date estimation and
#'  prediction.
#' @slot cutoff An \code{\link{integer}} vector giving the cutoff value.
#' @slot dimension An \code{\link{integer}} vector giving the CA dimensions
#'  kept.
#' @slot level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @slot row_events A four columns \code{\link{numeric}} matrix giving the
#'  predicted event dates for each archaeological assemblage, with the following
#'  columns:
#'  \describe{
#'   \item{date}{The event date estimation.}
#'   \item{lower}{The lower boundary of the confidence interval.}
#'   \item{upper}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot column_events A four columns \code{\link{numeric}} matrix giving the
#'  predicted event dates for each archaeological type or fabric, with the
#'  following columns:
#'  \describe{
#'   \item{date}{The event date estimation.}
#'   \item{lower}{The lower boundary of the confidence interval.}
#'   \item{upper}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot accumulation A two columns \code{\link{numeric}} matrix giving the
#'  point estimate of the accumulation dates and the corresponding error.
#' @section Subset:
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector. Returns the corresponding slot values.}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases DateEvent-class
.DateEvent <- setClass(
  Class = "DateEvent",
  slots = c(
    dates = "numeric",
    model = "lm",
    cutoff = "integer",
    keep = "integer"
  ),
  contains = "CA"
)

# RefineCA =======================================================================
#' Partial Bootstrap CA
#'
#' An S4 class to store partial bootstrap correspondence analysis results.
#' @slot row_chull A list of length three giving the vertices coordinates
#'  (\code{x}, \code{y}) of the samples convex hull and a identifier
#'  (\code{id}) to link each row to a sample.
#' @slot column_chull A list of length three giving the vertices coordinates
#'  (\code{x}, \code{y}) of the variables convex hull and a identifier
#'  (\code{id}) to link each row to a variable.
#' @slot row_lengths A list of two named \code{\link{numeric}} vectors giving the
#'  convex hull maximum dimension length of samples and variables, respectively.
#' @slot column_lengths A list of two named \code{\link{numeric}} vectors giving the
#'  convex hull maximum dimension length of samples and variables, respectively.
#' @slot cutoff A length-two \code{\link{numeric}} vector giving the cutoff
#'  value for samples and variables selection, respectively.
#' @slot keep A list of two \code{\link{integer}} vectors giving the subscript
#'  of the samples and variables to be kept, respectively.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{RefineCA} object.
#'  \describe{
#'   \item{\code{x[i, j, drop]}}{Extracts informations from a slot selected by
#'   subscript \code{i} thru \code{j} (see examples). \code{i} should be one of
#'   "\code{rows}" or "\code{columns}". Any unambiguous substring can be
#'   given. \code{j} is a \code{\link{numeric}}, \code{\link{integer}} or
#'   \code{\link{character}} vector or empty (missing) or \code{NULL}.
#'   Numeric values are coerced to \code{\link{integer}} as by
#'   \code{\link{as.integer}} (and hence truncated towards zero). Character
#'   vectors will be matched to the name of the elements. An empty index
#'   (a comma separated blank) indicates that all entries in that dimension are
#'   selected.}
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} should be one of "\code{id}",
#'   "\code{rows}", "\code{columns}", "\code{lengths}", "\code{cutoff}" or
#'   "\code{keep}". Any unambiguous substring can be given.}
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
#' @slot rows An \code{\link{integer}} vector giving the rows permutation.
#' @slot columns An \code{\link{integer}} vector giving the columns permutation.
#' @slot method A \code{\link{character}} string indicating the seriation
#'  method used.
#' @section Subset:
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} should be one of "\code{id}",
#'   "\code{rows}", "\code{columns}" or "\code{method}". Any unambiguous
#'   substring can be given.}
#'  }
#' @note
#'  Numeric values are coerced to \code{\link{integer}} as by
#'  \code{\link[base]{as.integer}} (and hence truncated towards zero).
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
