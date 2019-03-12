# DEFINITION, VALIDATION AND INITIALIZATION OF CLASSES
#' @include tabula.R utilities.R
NULL

# DEFINITION ===================================================================
## -----------------------------------------------------------------------------
#' Date model
#'
#' An S4 class to store the event and accumulation times of archaeological
#'  assemblages.
#' @slot counts A \eqn{m \times p}{m x p} \code{\link{numeric}} matrix of count
#'  data used for model fitting.
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
#'  dates for each archaeological assemblage, the corresponding confidence
#'  interval boundaries, standard error of the predicted dates and an
#'  identifier to link each row to an assemblage.
#' @slot columns A five columns \code{\link{data.frame}} giving the predicted
#'  event dates for each archaeological type or fabric, the corresponding
#'  confidence interval boundaries, standard error of the predicted dates and an
#'  identifier to link each row to a type or fabric.
#' @slot accumulation A two columns \code{\link{data.frame}} giving the point
#'  estimate of accumulation dates of archaeological assemblages and an
#'  identifier to link each row to an assemblage.
#' @author N. Frerebeau
#' @docType class
#' @aliases DateModel-class
setClass(
  Class = "DateModel",
  slots = c(
    counts = "matrix",
    dates = "data.frame",
    level = "numeric",
    model = "lm",
    residual = "numeric",
    rows = "data.frame",
    columns = "data.frame",
    accumulation = "data.frame"
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

## -----------------------------------------------------------------------------
#' Date model checking
#'
#' An S4 class to store the results of resampling methods for date model
#'  checking.
#' @slot jackknife A six columns \code{\link{data.frame}} giving the results of
#'  the resamping procedure (jackknifing) for each sample (in rows) with the
#'  following columns:
#'  \describe{
#'   \item{id}{An identifier.}
#'   \item{estimation}{The jackknife date estimate.}
#'   \item{earliest}{The lower boundary of the associated prediction interval.}
#'   \item{latest}{The upper boundary of the associated prediction interval.}
#'   \item{error}{The standard error of predicted means.}
#'   \item{bias}{The jackknife estimate of bias.}
#'  }
#' @slot bootstrap A six columns \code{\link{data.frame}} giving the boostrap
#'  distribution statistics for each replicated sample (in rows) with the
#'  following columns:
#'  \describe{
#'   \item{id}{An identifier.}
#'   \item{min}{Minimum value.}
#'   \item{Q05}{Sample quantile to 0.05 probability.}
#'   \item{mean}{Mean value.}
#'   \item{Q95}{Sample quantile to 0.95 probability.}
#'   \item{max}{Maximum value.}
#'  }
#' @author N. Frerebeau
#' @docType class
#' @aliases BootDate-class
setClass(
  Class = "BootDate",
  slots = c(jackknife = "data.frame",
            bootstrap = "data.frame")
)

## Numeric matrix --------------------------------------------------------------
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
#' An S4 class to represent a frequency matrix.
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

#' Co-occurrence matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @details
#'  A co-occurrence matrix is a symetric matrix with zeros on its main diagonal,
#'  which works out which pairs of taxa occur together in at least one sample
#' @note This class extends the \code{base} \link[base]{matrix}.
#' @seealso \link[base]{matrix}
#' @family logical matrix
#' @example inst/examples/ex-logical-class.R
#' @author N. Frerebeau
#' @docType class
#' @aliases OccurrenceMatrix-class
setClass(
  Class = "OccurrenceMatrix",
  contains = "LogicalMatrix"
)

# VALIDATION ===================================================================
## DateModel -------------------------------------------------------------------
setValidity(
  Class = "DateModel",
  method = function(object) {
    errors <- c()
    # Get data
    counts <- object@counts
    dates <- object@dates
    level <- object@level
    model <- object@model
    residual <- object@residual
    rows <- object@rows
    columns <- object@columns
    accumulation <- object@accumulation

    if (length(counts) != 0) {
      if (!is.numeric(counts))
        errors <- c(errors, "'counts' must be a numeric matrix.")
      if (any(is.na(counts)))
        errors <- c(errors, "'counts' must not contain NA values.")
    }
    if (length(level) != 0) {
      if (!is.numeric(level) | length(level) != 1 | any(is.na(level)))
        errors <- c(errors, "'level' must be a length-one numeric vector.")
      if (level <= 0 | level >= 1)
        errors <- c(errors, "'level' must be in the range of 0 to 1 (excluded).")
    }
    if (length(residual) != 0) {
      if (!is.numeric(residual) | any(is.na(residual)) | length(residual) != 1 | level < 0)
        errors <- c(errors, "'residual' must be a strictly positive numeric value.")
    }
    if (length(rows) != 0) {
      if (ncol(rows) != 5)
        errors <- c(errors, "'rows' must be a five columns data frame.")
      if (any(is.na(rows)))
        errors <- c(errors, "'rows' must not contain NA values.")
    }
    if (length(columns) != 0) {
      if (ncol(columns) != 5)
        errors <- c(errors, "'columns' must be a five columns data frame.")
      if (any(is.na(columns)))
        errors <- c(errors, "'columns' must not contain NA values.")
    }
    if (length(accumulation) != 0) {
      if (ncol(accumulation) != 2)
        errors <- c(errors, "'accumulation' must be a two columns data frame.")
      if (any(is.na(accumulation)))
        errors <- c(errors, "'columns' must not contain NA values.")
    }
    if (length(counts) != 0 & length(rows) != 0) {
      a <- nrow(counts)
      b <- nrow(rows)
      if (b != a)
        errors <- c(errors, paste("'rows' must be a 5 x", a, "data frame."))
    }
    if (length(counts) != 0 & length(columns) != 0) {
      a <- ncol(counts)
      b <- nrow(columns)
      if (b != a)
        errors <- c(errors, paste("'columns' must be a 5 x", a, "data frame."))
    }
    if (length(counts) != 0 & length(accumulation) != 0) {
      a <- nrow(counts)
      b <- nrow(accumulation)
      if (b != a)
        errors <- c(errors, paste("'accumulation' must be a 2 x", a, "data frame."))
    }
    # Return errors if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## PermutationOrder ------------------------------------------------------------
setValidity(
  Class = "PermutationOrder",
  method = function(object) {
    errors <- c()
    # Get data
    rows <- object@rows
    columns <- object@columns
    method <- object@method

    if (length(rows) != 0) {
      if (!is.integer(rows) | any(is.na(rows)) | !isPositive(rows))
        errors <- c(errors, "'rows' must be a vector of strictly positive integers.")
    }
    if (length(columns) != 0) {
      if (!is.integer(columns) | any(is.na(columns)) | !isPositive(columns))
        errors <- c(errors, "'columns' must be a vector of strictly positive integers.")
    }
    if (length(method) != 0) {
      if (length(method) != 1 | !is.character(method)) {
        errors <- c(errors, "'method' must be a single character string.")
      }
    }
    # Return errors if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## BootCA ----------------------------------------------------------------------
setValidity(
  Class = "BootCA",
  method = function(object) {
    errors <- c()
    # Get data
    rows <- object@rows
    columns <- object@columns
    lengths <- object@lengths
    cutoff <- object@cutoff
    keep <- object@keep

    if (length(rows) != 0) {
      if (ncol(rows) != 3) {
        errors <- c(errors, "'rows' must be a three columns data frame.")
      } else {
        if (!identical(colnames(rows), c("id", "x", "y"))) {
          errors <- c(errors, "'rows' has wrong column names.")
        } else {
          if (!is.numeric(rows$x) | !is.numeric(rows$y))
            errors <- c(errors, "'x' and 'y' columns of 'rows' must be of numeric type.")
        }
      }
      if (any(is.na(rows)))
        errors <- c(errors, "'rows' must not contain NA values.")
    }
    if (length(columns) != 0) {
      if (ncol(columns) != 3) {
        errors <- c(errors, "'columns' must be a three columns data frame.")
      } else {
        if (!identical(colnames(columns), c("id", "x", "y"))) {
          errors <- c(errors, "'columns' has wrong column names.")
        } else {
          if (!is.numeric(columns$x) | !is.numeric(columns$y))
            errors <- c(errors, "'x' and 'y' columns of 'columns' must be of numeric type.")
        }
      }
      if (any(is.na(columns)))
        errors <- c(errors, "'columns' must not contain NA values.")
    }
    if (length(lengths) != 0) {
      if (any(!is.numeric(lengths)) | any(is.na(lengths)))
        errors <- c(errors, "'length' must be a numeric vector")
    }
    if (length(cutoff) != 0) {
      if (length(cutoff) != 1 | !is.numeric(cutoff) | is.na(cutoff))
        errors <- c(errors, "'cutoff' must be a length-one numeric vector")
    }
    if (length(keep) != 0) {
      if (any(!is.numeric(keep)) | any(is.na(keep)))
        errors <- c(errors, "'keep' must be a numeric vector")
    }
    # Return errors if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## NumericMatrix ---------------------------------------------------------------
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    errors <- c()
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")

    # Check data
    if (length(data) != 0) {
      if (!is.numeric(data))
        errors <- c(errors, "numeric values are expected.")
      if (any(is.na(data)))
        errors <- c(errors, "NA values were detected.")
      if (any(is.infinite(data)))
        errors <- c(errors, "infinite numbers were detected.")
    }

    # Return errors if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## CountMatrix -----------------------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    errors <- c()
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    # Check data
    if (length(data) != 0) {
      if (sum(!isWholeNumber(data)) != 0)
        errors <- c(errors, "whole numbers are expected.")
      if (isBinary(data))
        errors <- c(errors, "you should consider using an incidence matrix.")
      if (!isPositive(data))
        errors <- c(errors, "positive values are expected.")
    }

    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## FrequencyMatrix -------------------------------------------------------------
setValidity(
  Class = "FrequencyMatrix",
  method = function(object) {
    errors <- c()
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    totals <- object@totals

    # Check data
    if (length(data) != 0) {
      if (!isEqual(rowSums(data, na.rm = TRUE)))
        errors <- c(errors, "constant row sums are expected.")
      if (isBinary(data))
        errors <- c(errors, "you should consider using an incidence matrix.")
      if (length(totals) != nrow(data))
        errors <- c(errors, paste("'totals' should be of length", nrow(data)))
      if (!isPositive(data))
        errors <- c(errors, "positive values are expected.")
    }

    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## SimilarityMatrix ------------------------------------------------------------
setValidity(
  Class = "SimilarityMatrix",
  method = function(object) {
    errors <- c()
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    # Check data
    if (length(data) != 0) {
      if (!isSquare(data))
        errors <- c(errors, "a square matrix is expected.")
      if (!isSymmetric(data))
        errors <- c(errors, "a symmetric matrix is expected.")
      if (!identical(rownames(data), colnames(data)))
        errors <- c(errors, "rows and columns should have the same names.")
    }

    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## LogicalMatrix ---------------------------------------------------------------
setValidity(
  Class = "LogicalMatrix",
  method = function(object) {
    errors <- c()
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    # Check data
    if (length(data) != 0) {
      if (!is.logical(data))
        errors <- c("logical values are expected.")
      if (any(is.na(data)))
        errors <- c(errors, "NA values were detected.")
    }

    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

## OccurrenceMatrix ------------------------------------------------------------
setValidity(
  Class = "OccurrenceMatrix",
  method = function(object) {
    errors <- c()
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    # Check data
    if (length(data) != 0) {
      if (!isSquare(data))
        errors <- c(errors, "a square matrix is expected.")
      if (!identical(rownames(data), colnames(data)))
        errors <- c(errors, "rows and columns should have the same names.")
    }

    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
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
## BootDate ----------------------------------------------------------------------
setMethod(
  f = "initialize",
  signature = "BootDate",
  definition = function(.Object, jackknife, bootstrap) {
    if (!missing(jackknife)) .Object@jackknife <- jackknife
    if (!missing(bootstrap)) .Object@bootstrap <- bootstrap
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
                        rows, columns, accumulation) {
    if (!missing(counts)) .Object@counts <- counts
    if (!missing(dates)) .Object@dates <- dates
    # FIXME: workaround to initialize empty instance
    .Object@model <- if (!missing(model)) model else stats::lm(0 ~ 0)
    if (!missing(level)) .Object@level <- level
    if (!missing(residual)) .Object@residual <- residual
    if (!missing(rows)) .Object@rows <- rows
    if (!missing(columns)) .Object@columns <- columns
    if (!missing(accumulation)) .Object@accumulation <- accumulation
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
