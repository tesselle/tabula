# CLASSES VALIDATION
#' @include AllClasses.R utilities.R
NULL

# BootCA =======================================================================
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
      if (ncol(lengths) != 2) {
        errors <- c(errors, "'lengths' must be a two columns data frame.")
      } else {
        if (!identical(colnames(lengths), c("id", "d"))) {
          errors <- c(errors, "'lengths' has wrong column names.")
        } else {
          if (!is.numeric(lengths$d))
            errors <- c(errors, "'d' column of 'lengths' must be of numeric type.")
        }
      }
      if (any(is.na(lengths)))
        errors <- c(errors, "'lengths' must not contain NA values.")
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

# DateModel ====================================================================
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

# PermutationOrder =============================================================
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

# NumericMatrix ================================================================
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

# LogicalMatrix ================================================================
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
