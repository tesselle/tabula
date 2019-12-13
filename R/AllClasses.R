# CLASSES DEFINITION AND INITIALIZATION
#' @include utilities.R
NULL

# DEFINITION ===================================================================
#' Diversity Index
#'
#' An S4 class to represent a diversity measure.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
#' @slot index A \code{\link{numeric}} vector giving the diversity index values.
#' @slot size A \code{\link{integer}} vector giving the sample sizes.
#' @slot jackknife A numeric \code{\link{matrix}} vector giving the jackknifed
#'  estimates.
#' @slot boostrap A numeric \code{\link{matrix}} vector giving the boostraped
#'  estimates.
#' @slot simulated A numeric \code{\link{matrix}} vector giving the diversity
#'  measures for the simulated assemblage.
#' @slot method A \code{\link{character}} string indicating the method used.
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
    id = "character",
    index = "numeric",
    size = "integer",
    jackknife = "matrix",
    boostrap = "matrix",
    simulated = "matrix",
    method = "character"
  )
)
#' @rdname DiversityIndex
#' @aliases HeterogeneityIndex HeterogeneityIndex-class
.HeterogeneityIndex <- setClass(
  Class = "HeterogeneityIndex",
  contains = "DiversityIndex"
)
#' @rdname DiversityIndex
#' @aliases EvennessIndex EvennessIndex-class
.EvennessIndex <- setClass(
  Class = "EvennessIndex",
  contains = "DiversityIndex"
)
#' @rdname DiversityIndex
#' @aliases RichnessIndex RichnessIndex-class
.RichnessIndex <- setClass(
  Class = "RichnessIndex",
  contains = "DiversityIndex"
)

## -----------------------------------------------------------------------------
#' Date Model
#'
#' An S4 class to store the event and accumulation times of archaeological
#'  assemblages.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
#' @slot counts A numeric matrix of count data.
#' @slot level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @slot model A \code{\link[stats:lm]{multiple linear model}}: the Gaussian
#'  multiple linear regression model fitted for event date estimation and
#'  prediction.
#' @slot rows A four columns \code{\link{numeric}} matrix giving the predicted
#'  event dates for each archaeological assemblage, with the following columns:
#'  \describe{
#'   \item{date}{The event date estimation.}
#'   \item{lower}{The lower boundary of the confidence interval.}
#'   \item{upper}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot columns A four columns \code{\link{numeric}} matrix giving the
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
#' @aliases DateModel-class
.DateModel <- setClass(
  Class = "DateModel",
  slots = c(
    id = "character",
    counts = "matrix",
    level = "numeric",
    model = "lm",
    rows = "matrix",
    columns = "matrix",
    accumulation = "matrix"
  )
)

## -----------------------------------------------------------------------------
#' Partial bootstrap CA
#'
#' An S4 class to store partial bootstrap correspondence analysis results.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
#' @slot rows A list of length three giving the vertices coordinates
#'  (\code{x}, \code{y}) of the samples convex hull and a identifier
#'  (\code{id}) to link each row to a sample.
#' @slot columns A list of length three giving the vertices coordinates
#'  (\code{x}, \code{y}) of the variables convex hull and a identifier
#'  (\code{id}) to link each row to a variable.
#' @slot lengths A list of two named \code{\link{numeric}} vectors giving the
#'  convex hull maximum dimension length of samples and variables, respectively.
#' @slot cutoff A length-two \code{\link{numeric}} vector giving the cutoff
#'  value for samples and variables selection, respectively.
#' @slot keep A list of two \code{\link{integer}} vectors giving the subscript
#'  of the samples and variables to be kept, respectively.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{BootCA} object.
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
#' @aliases BootCA-class
.BootCA <- setClass(
  Class = "BootCA",
  slots = c(
    id = "character",
    rows = "list",
    columns = "list",
    lengths = "list",
    cutoff = "numeric",
    keep = "list"
  )
)

## -----------------------------------------------------------------------------
#' Permutation order
#'
#' An S4 class to represent a permutation order.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
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
    id = "character",
    rows = "integer",
    columns = "integer",
    method = "character"
  )
)

# INITIALIZATION ===============================================================
## DateModel -------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "DateModel",
  definition = function(.Object, ..., id, counts, level, model,
                        rows, columns, accumulation) {

    mtx <- matrix(0, 0, 4, dimnames = list(NULL, c("date", "lower", "upper", "error")))
    acc <- matrix(0, 0, 2, dimnames = list(NULL, c("date", "error")))

    .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
    .Object@counts <- if (missing(counts)) matrix(0, 0, 0) else counts
    .Object@level <- if (missing(level)) numeric(1) else level
    .Object@model <- if (missing(model)) stats::lm(0 ~ 0) else model
    .Object@rows <- if (missing(rows)) mtx else rows
    .Object@columns <- if (missing(columns)) mtx else columns
    .Object@accumulation <- if (missing(accumulation)) acc else accumulation

    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)
## DiversityIndex --------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "DiversityIndex",
  definition = function(.Object, ..., id, index, size, jackknife, boostrap,
                        simulated, method) {

    jack <- matrix(0, 0, 3, dimnames = list(NULL, c("mean", "bias", "error")))
    boot <- matrix(0, 0, 5, dimnames = list(NULL, c("min","Q05", "mean", "Q95", "max")))
    sim <- matrix(0, 0, 4, dimnames = list(NULL, c("size", "mean", "lower", "upper")))

    .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
    .Object@index <- if (missing(index)) numeric(0) else index
    .Object@size <- if (missing(size)) integer(0) else as.integer(size)
    .Object@jackknife <- if (missing(jackknife)) jack else jackknife
    .Object@boostrap <- if (missing(boostrap)) boot else boostrap
    .Object@simulated <- if (missing(simulated)) sim else simulated
    .Object@method <- if (missing(method)) "unknown" else method

    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)
## BootCA ----------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "BootCA",
  definition = function(.Object, ..., id, rows, columns, lengths,
                        cutoff, keep) {

    .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
    if (missing(rows)) {
      rows <- list(id = factor(), x = numeric(0), y = numeric(0))
    } else {
      rows
    }
    .Object@rows <- rows
    if (missing(columns)) {
      columns <- list(id = factor(), x = numeric(0), y = numeric(0))
    } else {
      columns
    }
    .Object@columns <- columns
    .Object@lengths <- if (missing(lengths)) {
      list(numeric(0), numeric(0))
    } else {
      mapply(FUN = `names<-`,
             lengths, list(unique(rows$id), unique(columns$id)),
             SIMPLIFY = FALSE)
    }
    .Object@cutoff <- if (missing(cutoff)) c(0, 0) else cutoff
    .Object@keep <- if (missing(keep)) list(integer(0), integer(0)) else keep

    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)

## PermutationOrder ------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "PermutationOrder",
  definition = function(.Object, ..., id, rows, columns, method) {

    .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
    .Object@rows <- if (missing(rows)) integer(0) else rows
    .Object@columns <- if (missing(columns)) integer(0) else columns
    .Object@method <- if (missing(method)) "unknown" else method

    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)
