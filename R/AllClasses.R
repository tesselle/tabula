#' @include tabula.R utilities.R
NULL

# Classes definition ===========================================================
# Permutation order ------------------------------------------------------------
#' An S4 class to represent a permutation order
#'
#' @slot rows A \code{\link{integer}} vector.
#' @slot columns A \code{\link{integer}} vector.
#' @slot seriation A \code{\link{character}} vector.
#' @author N. Frerebeau
#' @docType class
#' @aliases PermutationOrder-class
setClass(
  Class = "PermutationOrder",
  slots = c(rows = "integer",
            columns = "integer",
            method = "character")
)

# Numeric matrix ---------------------------------------------------------------
setClass(
  Class = "NumericMatrix",
  contains = "matrix"
)

#' S4 classes to represent numeric matrix
#'
#' @inheritParams base::matrix
#' @seealso \link[base]{matrix}
#' @author N. Frerebeau
#' @docType class
#' @name NumericMatrix
#' @rdname NumericMatrix
NULL

#' @description
#'  \code{CountMatrix} represents a count matrix.
#' @rdname NumericMatrix
#' @aliases CountMatrix-class
setClass(
  Class = "CountMatrix",
  contains = "NumericMatrix"
)

#' @description
#'  \code{FrequencyMatrix} represents a frequency matrix.
#' @slot total A \code{\link{numeric}} vector.
#' @rdname NumericMatrix
#' @aliases FrequencyMatrix-class
setClass(
  Class = "FrequencyMatrix",
  slots = c(totals = "numeric"),
  contains = "NumericMatrix"
)

# Logical matrix ---------------------------------------------------------------
setClass(
  Class = "LogicalMatrix",
  contains = "matrix"
)

#' S4 classes to represent logical matrix
#'
#' @inheritParams base::matrix
#' @seealso \link[base]{matrix}
#' @author N. Frerebeau
#' @docType class
#' @name LogicalMatrix
#' @rdname LogicalMatrix
NULL

#' @description
#'  \code{IncidenceMatrix} represents an incidence (presence/absence) matrix.
#' @rdname LogicalMatrix
#' @aliases IncidenceMatrix-class
setClass(
  Class = "IncidenceMatrix",
  contains = "LogicalMatrix"
)

#' @description
#'  \code{StratigraphicMatrix} represents a stratigraphic matrix.
#' @rdname LogicalMatrix
#' @aliases StratigraphicMatrix-class
setClass(
  Class = "StratigraphicMatrix",
  contains = "LogicalMatrix"
)

# Classes validation ===========================================================
# PermutationOrder class -------------------------------------------------------
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
    if (length(rows) != 0 | length(columns) != 0) {
      if (length(method) == 1) {
        if (!is.character(method))
          errors <- c(errors, "a character string is expected")
      } else {
        errors <- c(errors, "should be of length 1")
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

# NumericMatrix class ----------------------------------------------------------
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

# CountMatrix class ---------------------------------------------------------
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

# FrequencyMatrix class --------------------------------------------------------
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

# LogicalMatrix class ----------------------------------------------------------
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
setValidity(
  Class = "StratigraphicMatrix",
  method = function(object) {
    errors <- c()
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    if (length(data) != 0) {
      if (nrow(data) != ncol(data))
        errors <- c(errors, "a symetric matrix is expected")
      if (rownames(data) != colnames(data))
        errors <- c(errors, "rows and cols should have the same names")
    }
    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(errors, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

# Classes constructor ==========================================================
setMethod(
  f = "initialize",
  signature = "PermutationOrder",
  definition = function(.Object, ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    methods::validObject(.Object)
    if (getOption("verbose")) {
      message(paste(class(.Object), "instance initialized.", sep = " "))
    }
    return(.Object)
  }
)

initialize_matrix <- function(.Object, ...) {
  .Object <- methods::callNextMethod(.Object, ...)
  methods::validObject(.Object)
  if (getOption("verbose")) {
    message(paste(class(.Object), "instance initialized.", sep = " "))
  }
  return(.Object)
}
setMethod(
  f = "initialize",
  signature = "CountMatrix",
  definition = initialize_matrix
)
setMethod(
  f = "initialize",
  signature = "FrequencyMatrix",
  definition = initialize_matrix
)
setMethod(
  f = "initialize",
  signature = "IncidenceMatrix",
  definition = initialize_matrix
)
setMethod(
  f = "initialize",
  signature = "StratigraphicMatrix",
  definition = initialize_matrix
)

# Show methods =================================================================
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
