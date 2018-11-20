#' @include tabula.R utilities.R
NULL

# Class definitions ============================================================
## -----------------------------------------------------------------------------
#' Permutation order
#'
#' An S4 class to represent a permutation order.
#' @slot rows A \code{\link{integer}} vector giving the rows permutation.
#' @slot columns A \code{\link{integer}} vector giving the columns permutation.
#' @slot seriation A \code{\link{character}} vector indicating the seriation
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
#' @slot lengths A \code{\link{numeric}} vector giving the convex hull
#'  maximum dimension length of each sample.
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
            lengths = "numeric",
            cutoff = "numeric",
            keep = "numeric")
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
#' @note
#'  This class extends the \code{base} \link[base]{matrix}.
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

# Class validation =============================================================
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
      if (!is.integer(rows))
        errors <- c(errors, "whole numbers are expected")
      if (any(is.na(rows)))
        errors <- c(errors, "NA values were detected")
      if (!any(is.nan(rows)))
        if (any(rows <= 0))
          errors <- c(errors, "strictly positive values are expected")
    }
    if (length(columns) != 0) {
      if (!is.integer(columns))
        errors <- c(errors, "whole numbers are expected")
      if (any(is.na(columns)))
        errors <- c(errors, "NA values were detected")
      if (!any(is.nan(columns)))
        if (any(columns <= 0))
          errors <- c(errors, "strictly positive values are expected")
    }
    if (length(method) != 0) {
      if (length(method) != 1) {
        errors <- c(errors, "a single character string is expected")
      } else {
        if (!is.character(method))
          errors <- c(errors, "a character string is expected")
      }
    }
    # Return errors if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
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
        errors <- c(errors, "wrong column dimension")
      } else {
        if (!identical(colnames(rows), c("id", "x", "y"))) {
          errors <- c(errors, "wrong column names")
        } else {
          if (!is.numeric(rows$x) | !is.numeric(rows$y))
            errors <- c(errors, "numeric values are expected")
        }
      }
      if (any(is.na(rows)))
        errors <- c(errors, "NA values were detected")
    }
    if (length(columns) != 0) {
      if (ncol(columns) != 3) {
        errors <- c(errors, "wrong column dimension")
      } else {
        if (!identical(colnames(columns), c("id", "x", "y"))) {
          errors <- c(errors, "wrong column names")
        } else {
          if (!is.numeric(columns$x) | !is.numeric(columns$y))
            errors <- c(errors, "numeric values are expected")
        }
      }
      if (any(is.na(columns)))
        errors <- c(errors, "NA values were detected")
    }
    if (length(lengths) != 0) {
      if (any(!is.numeric(lengths)) | any(is.na(lengths)))
        errors <- c(errors, "numeric values are expected")
    }
    if (length(cutoff) != 0) {
      if (length(cutoff) != 1) {
        errors <- c(errors, "a single value is expected")
      } else {
        if (!is.numeric(cutoff) | is.na(cutoff))
          errors <- c(errors, "a numeric value is expected")
      }
    }
    if (length(keep) != 0) {
      if (any(!is.numeric(keep)) | any(is.na(keep)))
        errors <- c(errors, "numeric values are expected")
    }
    # Return errors if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
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
    if (length(data) != 0) {
      if (!is.numeric(data))
        errors <- c(errors, "numeric values are expected")
      if (any(is.na(data)))
        errors <- c(errors, "NA values were detected")
      if (any(is.infinite(data)))
        errors <- c(errors, "infinite numbers were detected")
      if (!any(is.nan(data)))
        if (any(data < 0))
          errors <- c(errors, "positive values are expected")
    }
    # Return errors if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
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
    if (length(data) != 0) {
      if (sum(!isWholeNumber(data)) != 0)
        errors <- c(errors, "whole numbers are expected")
      if (isBinary(data))
        errors <- c(errors, "you should consider using an incidence matrix")
    }
    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
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
    if (length(data) != 0) {
      if (!isEqual(rowSums(data, na.rm = TRUE)))
        errors <- c(errors, "frequencies are expected")
      if (isBinary(data))
        errors <- c(errors, "you should consider using an incidence matrix")
      if (length(totals) != nrow(data))
        errors <- c(errors, "wrong row sums")
    }
    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
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
    if (length(data) != 0) {
      if (!is.logical(data))
        errors <- c("logical values are expected")
      if (any(is.na(data)))
        errors <- c(errors, "NA values were detected")
    }
    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
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

    if (length(data) != 0) {
      if (nrow(data) != ncol(data))
        errors <- c(errors, "a square matrix is expected")
      if (!identical(rownames(data), colnames(data)))
        errors <- c(errors, "rows and columns should have the same names")
    }
    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

# Class constructors ===========================================================
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
setMethod("initialize", "IncidenceMatrix", initialize_matrix)
setMethod("initialize", "OccurrenceMatrix", initialize_matrix)

# Show =========================================================================
## PermutationOrder ------------------------------------------------------------
setMethod(
  f = "show",
  signature = "PermutationOrder",
  definition = function(object) {
    cat("Permutation order for matrix seriation:", "\n",
        "  Row order:", object@rows, "\n",
        "  Column order:", object@columns, "\n",
        "  Method:", object@method,
        sep = " "
    )
  }
)
## Numeric matrix --------------------------------------------------------------
setMethod(
  f = "show",
  signature = "CountMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(paste(m, "x", p, "count data matrix:", sep = " "), "\n", sep = " ")
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "FrequencyMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(paste(m, "x", p, "frequency data matrix:", sep = " "), "\n", sep = " ")
    print(data)
  }
)
## Logical matrix --------------------------------------------------------------
setMethod(
  f = "show",
  signature = "IncidenceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(paste(m, "x", p, "presence/absence data matrix:", sep = " "), "\n",
        sep = " ")
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "OccurrenceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(paste(m, "x", p, "co-occurrence matrix:", sep = " "), "\n",
        sep = " ")
    print(data)
  }
)

# Accessors ====================================================================
#' @export
#' @param x A \code{PermutationOrder} object from which to extract element(s).
#' @param i A \code{\link{character}} string specifying elements to extract.
#'  Character vectors will be matched to the name of the slots.
#' @describeIn PermutationOrder Returns information about the individual slots.
#' @aliases [[,PermutationOrder-method
setMethod(
  f = "[[",
  signature = "PermutationOrder",
  definition = function(x, i){
    i <- match.arg(i, choices = methods::slotNames("PermutationOrder"),
                   several.ok = FALSE)
    slot <- slot(x, i)
    return(slot)
  }
)

#' @export
#' @param x A \code{BootCA} object from which to extract element(s).
#' @param i A \code{\link{character}} string specifying elements to extract.
#'  Character vectors will be matched to the name of the slots.
#' @describeIn BootCA Returns information about the individual slots.
#' @aliases [[,BootCA-method
setMethod(
  f = "[[",
  signature = "BootCA",
  definition = function(x, i){
    i <- match.arg(i, choices = methods::slotNames("BootCA"),
                   several.ok = FALSE)
    slot <- slot(x, i)
    return(slot)
  }
)

#' Accessors
#'
#' @param x An object.
#' @author N. Frerebeau
#' @docType methods
#' @name accessors
#' @rdname accessors
NULL

#' @rdname accessors
setGeneric("columns", function(x) standardGeneric("columns"))

#' @rdname accessors
setGeneric("method", function(x) standardGeneric("method"))

#' @rdname accessors
setGeneric("rows", function(x) standardGeneric("rows"))

#' @rdname accessors
setGeneric("totals", function(x) standardGeneric("totals"))

#' @export
#' @describeIn PermutationOrder Returns the rows permutation.
#' @aliases rows,PermutationOrder-method
setMethod("rows", "PermutationOrder", function(x) x@rows)

#' @export
#' @describeIn BootCA Returns the convex hull vertice coordinates for each
#'  individual.
#' @aliases rows,BootCA-method
setMethod("rows", "BootCA", function(x) x@rows)

#' @export
#' @describeIn PermutationOrder Returns the columns permutation.
#' @aliases columns,PermutationOrder-method
setMethod("columns", "PermutationOrder", function(x) x@columns)

#' @export
#' @describeIn BootCA Returns the convex hull vertice coordinates for each
#'  variable.
#' @aliases columns,BootCA-method
setMethod("columns", "BootCA", function(x) x@columns)

#' @export
#' @describeIn PermutationOrder Returns the method used for seriation.
#' @aliases method,PermutationOrder-method
setMethod("method", "PermutationOrder", function(x) x@method)

#' @export
#' @describeIn FrequencyMatrix Returns the row sums (counts).
#' @aliases totals,FrequencyMatrix-method
setMethod("totals", "FrequencyMatrix", function(x) x@totals)
