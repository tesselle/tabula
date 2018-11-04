#' @include utilities.R
NULL

# Classes definition ===========================================================
# Numeric matrix ---------------------------------------------------------------
#' S4 classes to represent numeric matrix
#'
#' @inheritParams base::matrix
#' @author N. Frerebeau
#' @docType class
#' @name NumericMatrix
#' @rdname NumericMatrix
NULL

.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  contains = c("matrix")
)
# Count matrix
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = "NumericMatrix"
)
# Frequency matrix
.FrequencyMatrix <- setClass(
  Class = "FrequencyMatrix",
  slots = c(totals = "numeric"),
  contains = "NumericMatrix"
)

# Logical matrix ---------------------------------------------------------------
#' S4 classes to represent logical matrix
#'
#' @inheritParams base::matrix
#' @author N. Frerebeau
#' @docType class
#' @name LogicalMatrix
#' @rdname LogicalMatrix
NULL

.LogicalMatrix <- setClass(
  Class = "LogicalMatrix",
  contains = c("matrix")
)
# Incidence matrix (presence/absence data)
.IncidenceMatrix <- setClass(
  Class = "IncidenceMatrix",
  contains = "LogicalMatrix"
)
# Stratigraphic matrix
.StratigraphicMatrix <- setClass(
  Class = "StratigraphicMatrix",
  contains = "LogicalMatrix"
)

# Classes union ================================================================
setClassUnion(name = "MatrixOrDataFrame", members = c("matrix", "data.frame"))

# Classes validation ===========================================================
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
      if (!any(is.nan(data))) if (any(data < 0))
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
    data <- S3Part(object, strictS3 = TRUE, "matrix")
    if (length(data) != 0) {
      # if (sum(!isWholeNumber(data)) != 0)
      #   errors <- c(errors, "whole numbers are expected")
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
    data <- S3Part(object, strictS3 = TRUE, "matrix")
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
    data <- S3Part(object, strictS3 = TRUE, "matrix")
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
    data <- S3Part(object, strictS3 = TRUE, "matrix")
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
initialize_matrix <- function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  validObject(.Object)
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
