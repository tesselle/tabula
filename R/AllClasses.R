# CLASSES DEFINITION AND INITIALIZATION
#' @include tabula.R
NULL

# DEFINITION ===================================================================
## -----------------------------------------------------------------------------
#' Date model
#'
#' An S4 class to store the event and accumulation times of archaeological
#'  assemblages as well as the results of resampling methods for date model
#'  checking.
#' @slot counts A numeric matrix of count data.
#' @slot dates A two columns \code{\link{data.frame}} giving the known dates
#'  used for model fitting and an identifier to link each row to an assemblage.
#' @slot level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @slot model A \code{\link[stats:lm]{multiple linear model}}: the Gaussian
#'  multiple linear regression model fitted for event date estimation and
#'  prediction.
#' @slot residual A length-one \code{\link{numeric}} vector giving the residual
#'  standard deviation.
#' @slot rows A five columns \code{\link{data.frame}} giving the predicted event
#'  dates for each archaeological assemblage, with the following columns:
#'  \describe{
#'   \item{id}{An identifier to link each row to an assemblage.}
#'   \item{estimation}{The event date estimation.}
#'   \item{earliest}{The lower boundary of the confidence interval.}
#'   \item{latest}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot columns A five columns \code{\link{data.frame}} giving the predicted
#'  event dates for each archaeological type or fabric, with the following
#'  columns:
#'  \describe{
#'   \item{id}{An identifier to link each row to an assemblage.}
#'   \item{estimation}{The event date estimation.}
#'   \item{earliest}{The lower boundary of the confidence interval.}
#'   \item{latest}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot accumulation A two columns \code{\link{data.frame}} giving the point
#'  estimate of accumulation dates of archaeological assemblages and an
#'  identifier to link each row to an assemblage.
#' @slot jackknife A six columns \code{\link{data.frame}} giving the results of
#'  the resamping procedure (jackknifing fabrics) for each assemblage (in rows)
#'  with the following columns:
#'  \describe{
#'   \item{id}{An identifier to link each row to an assemblage.}
#'   \item{estimation}{The jackknife event date estimate.}
#'   \item{earliest}{The lower boundary of the associated prediction interval.}
#'   \item{latest}{The upper boundary of the associated prediction interval.}
#'   \item{error}{The standard error of predicted means.}
#'   \item{bias}{The jackknife estimate of bias.}
#'  }
#' @slot bootstrap A six columns \code{\link{data.frame}} giving the boostrap
#'  distribution statistics for each replicated assemblage (in rows)
#'  with the following columns:
#'  \describe{
#'   \item{id}{An identifier to link each row to an assemblage.}
#'   \item{min}{Minimum value.}
#'   \item{Q05}{Sample quantile to 0.05 probability.}
#'   \item{mean}{Mean value (event date).}
#'   \item{Q95}{Sample quantile to 0.95 probability.}
#'   \item{max}{Maximum value.}
#'  }
#' @param x A \code{DateModel} object from which to extract element(s).
#' @param i,j Indices specifying elements to extract.
#'  \code{i} is a \code{\link{character}} string matching to the name of a slot.
#'  \code{j} can be \code{\link{missing}} or \code{\link{NULL}},
#'  a \code{\link{numeric}} or \code{\link{character}} vector.
#'  Numeric values are coerced to \code{\link{integer}} as by
#'  \code{\link{as.integer}} (and hence truncated towards zero).
#'  Character vectors will be matched to the names of the object.
#' @param drop A \code{\link{logical}} scalar: should the result be coerced to
#'  the lowest possible dimension?
#' @author N. Frerebeau
#' @docType class
#' @aliases DateModel-class
setClass(
  Class = "DateModel",
  slots = c(
    level = "numeric",
    model = "lm",
    residual = "numeric",
    counts = "matrix",
    dates = "data.frame",
    rows = "data.frame",
    columns = "data.frame",
    accumulation = "data.frame",
    jackknife = "data.frame",
    bootstrap = "data.frame"
  )
)

## -----------------------------------------------------------------------------
#' Permutation order
#'
#' An S4 class to represent a permutation order.
#' @slot rows An \code{\link{integer}} vector giving the rows permutation.
#' @slot columns An \code{\link{integer}} vector giving the columns permutation.
#' @slot seriation A \code{\link{character}} string indicating the seriation
#'  method used.
#' @author N. Frerebeau
#' @docType class
#' @aliases PermutationOrder-class
setClass(
  Class = "PermutationOrder",
  slots = c(rows = "integer",
            columns = "integer",
            method = "character")
)

## -----------------------------------------------------------------------------
#' Partial bootstrap CA
#'
#' An S4 class to store partial bootstrap correspondance analysis results.
#' @slot rows A three columns \code{\link{data.frame}} giving the vertices
#'  coordinates of the samples convex hull and a identifier to link each row to
#'  a sample.
#' @slot columns A three columns \code{\link{data.frame}} giving the vertices
#'  coordinates of the variables convex hull and a identifier to link each row
#'  to a variable.
#' @slot lengths A two columns \code{\link{data.frame}} giving the convex hull
#'  maximum dimension length of each sample and a identifier to link each row to
#'  a sample.
#' @slot cutoff A length-one \code{\link{numeric}} vector giving the cutoff
#'  value for sample selection.
#' @slot keep A named \code{\link{numeric}} vector giving the subscript of
#'  the samples to be kept.
#' @param x A \code{BootCA} object from which to extract element(s).
#' @param i,j Indices specifying elements to extract.
#'  \code{i} is a \code{\link{character}} string matching to the name of a slot.
#'  \code{j} can be \code{\link{missing}} or \code{\link{NULL}},
#'  a \code{\link{numeric}} or \code{\link{character}} vector.
#'  Numeric values are coerced to \code{\link{integer}} as by
#'  \code{\link{as.integer}} (and hence truncated towards zero).
#'  Character vectors will be matched to the names of the object.
#' @author N. Frerebeau
#' @docType class
#' @aliases BootCA-class
setClass(
  Class = "BootCA",
  slots = c(rows = "data.frame",
            columns = "data.frame",
            lengths = "data.frame",
            cutoff = "numeric",
            keep = "numeric")
)

## Numeric matrix --------------------------------------------------------------
#' Numeric matrix
#'
#' An S4 class to represent a numeric matrix.
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
#' @docType class
#' @aliases NumericMatrix-class
#' @keywords internal
#' @noRd
setClass(
  Class = "NumericMatrix",
  contains = "matrix"
)

#' Count matrix
#'
#' An S4 class to represent a count matrix.
#' @inheritParams base::matrix
#' @details
#'  Numeric values are coerced to \code{\link{integer}} as by
#'  \code{\link[base]{as.integer}} (and hence truncated towards zero).
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
#' @family abundance matrix
#' @example inst/examples/ex-abundance-class.R
#' @author N. Frerebeau
#' @docType class
#' @aliases CountMatrix-class
setClass(
  Class = "CountMatrix",
  contains = "NumericMatrix"
)

#' Frequency matrix
#'
#' An S4 class to represent a relative frequency matrix.
#' @param x A \code{FrequencyMatrix} object from which to extract element.
#' @slot total A \code{\link{numeric}} vector.
#' @details
#'  To ensure data integrity, a \code{FrequencyMatrix} can only be created by
#'  coercion from a \linkS4class{CountMatrix} (see examples).
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
#' @family abundance matrix
#' @example inst/examples/ex-abundance-class.R
#' @author N. Frerebeau
#' @docType class
#' @aliases FrequencyMatrix-class
setClass(
  Class = "FrequencyMatrix",
  slots = c(totals = "numeric"),
  contains = "NumericMatrix"
)

#' Co-occurrence matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @details
#'  A co-occurrence matrix is a symetric matrix with zeros on its main diagonal,
#'  which works out how many times (expressed in percent) each pairs of taxa
#'  occur together in at least one sample.
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
#' @family abundance matrix
#' @example inst/examples/ex-abundance-class.R
#' @author N. Frerebeau
#' @docType class
#' @aliases OccurrenceMatrix-class
setClass(
  Class = "OccurrenceMatrix",
  contains = "NumericMatrix"
)

#' Similarity matrix
#'
#' An S4 class to represent a (dis)similarity matrix.
#' @param x A \code{SimilarityMatrix} object from which to extract element.
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
# @family
# @example
#' @author N. Frerebeau
#' @docType class
#' @aliases SimilarityMatrix-class
setClass(
  Class = "SimilarityMatrix",
  slots = c(method = "character"),
  contains = "NumericMatrix"
)

## Logical matrix --------------------------------------------------------------
#' Logical matrix
#'
#' An S4 class to represent a logical matrix.
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
#' @docType class
#' @aliases LogicalMatrix-class
#' @keywords internal
#' @noRd
setClass(
  Class = "LogicalMatrix",
  contains = "matrix"
)

#' Incidence matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams base::matrix
#' @details
#'  Numeric values are coerced to \code{\link{logical}} as by
#'  \code{\link[base]{as.logical}}.
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
#' @family logical matrix
#' @example inst/examples/ex-logical-class.R
#' @author N. Frerebeau
#' @docType class
#' @aliases IncidenceMatrix-class
setClass(
  Class = "IncidenceMatrix",
  contains = "LogicalMatrix"
)

# INITIALIZATION ===============================================================
## BootCA ----------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "BootCA",
  definition = function(.Object, rows, columns, lengths, cutoff, keep) {
    if (!missing(rows)) .Object@rows <- rows
    if (!missing(columns)) .Object@columns <- columns
    if (!missing(lengths)) .Object@lengths <- lengths
    if (!missing(cutoff)) .Object@cutoff <- cutoff
    if (!missing(keep)) .Object@keep <- keep
    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
    }
    return(.Object)
  }
)
## DateModel -------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "DateModel",
  definition = function(.Object, counts, dates, model, level, residual,
                        rows, columns, accumulation,
                        jackknife, bootstrap) {
    if (!missing(counts)) .Object@counts <- counts
    if (!missing(dates)) .Object@dates <- dates
    # FIXME: workaround to initialize empty instance
    .Object@model <- if (!missing(model)) model else stats::lm(0 ~ 0)
    if (!missing(level)) .Object@level <- level
    if (!missing(residual)) .Object@residual <- residual
    if (!missing(rows)) .Object@rows <- rows
    if (!missing(columns)) .Object@columns <- columns
    if (!missing(accumulation)) .Object@accumulation <- accumulation
    if (!missing(jackknife)) .Object@jackknife <- jackknife
    if (!missing(bootstrap)) .Object@bootstrap <- bootstrap
    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
    }
    return(.Object)
  }
)
## PermutationOrder ------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "PermutationOrder",
  definition = function(.Object, rows, columns, method) {
    if (!missing(rows)) .Object@rows <- rows
    if (!missing(columns)) .Object@columns <- columns
    if (!missing(method)) .Object@method <- method
    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
    }
    return(.Object)
  }
)

## *Matrix ---------------------------------------------------------------------
initialize_matrix <- function(.Object, ...) {
  .Object <- methods::callNextMethod(.Object, ...)
  methods::validObject(.Object)
  if (getOption("verbose")) {
    message(paste(class(.Object), "instance initialized.", sep = " "))
  }
  return(.Object)
}
setMethod("initialize", "CountMatrix", initialize_matrix)
setMethod("initialize", "FrequencyMatrix", initialize_matrix)
setMethod("initialize", "SimilarityMatrix", initialize_matrix)
setMethod("initialize", "IncidenceMatrix", initialize_matrix)
setMethod("initialize", "OccurrenceMatrix", initialize_matrix)

# CREATE =======================================================================
#' Matrix constructor
#'
#' @inheritParams base::matrix
#' @param rows A \code{\link{logical}} scalar indicating if the number of rows is
#'  unspecified.
#' @param cols A \code{\link{logical}} scalar indicating if the number of columns
#'  is unspecified.
#' @return A \link{\code{matrix}}.
#' @noRd
buildMatrix <- function(data, nrow, ncol, byrow, dimnames,
                        rows = FALSE, cols = FALSE) {
  k <- length(data)
  if (rows) nrow <- k / ncol
  if (cols) ncol <- k / nrow
  if (is.null(dimnames)) {
    dimnames <- list(1:nrow, paste("V", 1:ncol, sep = ""))
  } else {
    if (is.null(dimnames[[1]])) dimnames[[1]] <- 1:nrow
    if (is.null(dimnames[[2]])) dimnames[[2]] <- paste("V", 1:ncol, sep = "")
  }
  M <- matrix(data, nrow, ncol, byrow, dimnames)
  return(M)
}

#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  M <- buildMatrix(as.integer(data), nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  methods::new("CountMatrix", M)
}

# @export
# @rdname FrequencyMatrix-class
# FrequencyMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
#                             dimnames = NULL) {
#   M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
#                    missing(nrow), missing(ncol))
#   totals <- rowSums(M)
#   M <- M / totals
#   methods::new("FrequencyMatrix", M, totals = totals)
# }

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  data <- as.logical(data)
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  methods::new("IncidenceMatrix", M)
}
