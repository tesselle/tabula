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
      if (ncol(rows) != 3)
        errors <- c(errors, paste(sQuote("rows"), "must be a three columns data frame."))
      rows_names <- c("id", "x", "y")
      if (!identical(colnames(rows), rows_names))
        errors <- c(errors, paste(sQuote("rows"), "column names must be",
                                  paste(dQuote(rows_names), collapse = ", ")))
      rows_num <- apply(X = rows[, -1], MARGIN = 1, FUN = is.numeric)
      if (!any(rows_num))
        errors <- c(errors, paste(sQuote("rows"), "must be of numeric type."))
      if (anyNA(rows))
        errors <- c(errors, paste(sQuote("rows"), "must not contain missing values."))
    }
    if (length(columns) != 0) {
      if (ncol(columns) != 3)
        errors <- c(errors, paste(sQuote("columns"), "must be a three columns data frame."))
      columns_names <- c("id", "x", "y")
      if (!identical(colnames(columns), columns_names))
        errors <- c(errors, paste(sQuote("columns"), "column names must be",
                                  paste(dQuote(columns_names), collapse = ", ")))
      columns_num <- apply(X = columns[, -1], MARGIN = 1, FUN = is.numeric)
      if (!any(columns_num))
        errors <- c(errors, paste(sQuote("columns"), "must be of numeric type."))
      if (anyNA(columns))
        errors <- c(errors, paste(sQuote("columns"), "must not contain missing values."))
    }
    if (length(lengths) != 0) {
      if (ncol(lengths) != 2)
        errors <- c(errors, paste(sQuote("lengths"), "must be a two columns data frame."))
      lengths_names <- c("id", "d")
      if (!identical(colnames(lengths), lengths_names))
        errors <- c(errors, paste(sQuote("lengths"), "column names must be",
                                  paste(dQuote(lengths_names), collapse = ", ")))
      lengths_num <- apply(X = lengths[, -1, drop = FALSE], MARGIN = 1, FUN = is.numeric)
      if (!any(lengths_num))
        errors <- c(errors, paste(sQuote("lengths"), "must be of numeric type."))
      if (anyNA(lengths))
        errors <- c(errors, paste(sQuote("lengths"), "must not contain missing values."))
    }
    if (length(cutoff) != 0) {
      if (length(cutoff) != 1 | !is.numeric(cutoff) | anyNA(cutoff))
        errors <- c(errors, paste(dQuote("cutoff"), "must be a length-one numeric vector"))
    }
    if (length(keep) != 0) {
      if (!is.numeric(keep))
        errors <- c(errors, paste(sQuote("keep"), "must be a numeric vector."))
      if (anyNA(keep))
        errors <- c(errors, paste(sQuote("keep"), "must not contain missing values."))
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
    dates <- object@dates
    level <- object@level
    model <- object@model
    residual <- object@residual
    rows <- object@rows
    columns <- object@columns
    accumulation <- object@accumulation
    jackknife <- object@jackknife
    bootstrap <- object@bootstrap

    if (length(level) != 0) {
      if (length(level) != 1 | anyNA(level))
        errors <- c(errors, paste(sQuote("level"), "must be a length-one numeric vector."))
      else
        if (level <= 0 | level >= 1)
          errors <- c(errors, paste(sQuote("level"), "must be in the range of 0 to 1 (excluded)."))
    }
    if (length(residual) != 0) {
      if (length(residual) != 1 | anyNA(residual))
        errors <- c(errors, paste(sQuote("residual"), "must be a strictly positive numeric value."))
    }
    if (length(rows) != 0) {
      if (ncol(rows) != 5)
        errors <- c(errors, paste(sQuote("rows"), "must be a five columns data frame."))
      if (anyNA(rows))
        errors <- c(errors, paste(sQuote("rows"), "must not contain missing values."))
    }
    if (length(columns) != 0) {
      if (ncol(columns) != 5)
        errors <- c(errors, paste(sQuote("columns"), "must be a five columns data frame."))
      if (anyNA(columns))
        errors <- c(errors, paste(sQuote("columns"), "must not contain missing values."))
    }
    if (length(accumulation) != 0) {
      if (ncol(accumulation) != 2)
        errors <- c(errors, paste(sQuote("accumulation"), "must be a two columns data frame."))
      if (anyNA(accumulation))
        errors <- c(errors, paste(sQuote("accumulation"), "must not contain missing values."))
      if (length(rows) != 0) {
        a <- nrow(rows)
        b <- nrow(accumulation)
        if (b != a)
          errors <- c(errors, paste(sQuote("accumulation"), "must be a 5 x", a, "data frame."))
      }
    }
    if (length(jackknife) != 0) {
      if (ncol(jackknife) != 6)
        errors <- c(errors, paste(sQuote("jackknife"), "must be a six columns data frame."))
      if (anyNA(jackknife))
        errors <- c(errors, paste(sQuote("jackknife"), "must not contain missing values."))
      is_num <- apply(X = jackknife[, -1], MARGIN = 2, FUN = is.numeric)
      if (!any(is_num))
        errors <- c(errors, paste(sQuote("jackknife"), "must contain numeric values."))
      if (length(rows) != 0) {
        a <- nrow(rows)
        b <- nrow(jackknife)
        if (b != a)
          errors <- c(errors, paste(sQuote("jackknife"), "must be a 6 x", a, "data frame."))
      }
    }
    if (length(bootstrap) != 0) {
      if (ncol(bootstrap) != 6)
        errors <- c(errors, paste(sQuote("bootstrap"), "must be a six columns data frame."))
      if (anyNA(bootstrap))
        errors <- c(errors, paste(sQuote("bootstrap"), "must not contain missing values."))
      is_num <- apply(X = bootstrap[, -1], MARGIN = 2, FUN = is.numeric)
      if (!any(is_num))
        errors <- c(errors, paste(sQuote("bootstrap"), "must contain numeric values."))
      if (length(rows) != 0) {
        a <- nrow(rows)
        b <- nrow(bootstrap)
        if (b != a)
          errors <- c(errors, paste(sQuote("bootstrap"), "must be a 6 x", a, "data frame."))
      }
    }
    if (length(jackknife) != 0 & length(bootstrap) != 0)
      if (nrow(jackknife) != nrow(bootstrap))
        errors <- c(errors, paste(sQuote("jackknife"), "and", sQuote("bootstrap"), "must have the same number of rows."))
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
      if (!is.integer(rows) | anyNA(rows) | !isPositive(rows, strict = TRUE))
        errors <- c(errors, paste(sQuote("rows"), "must be a vector of strictly positive integers."))
      if (length(columns) == 0)
        errors <- c(errors, paste(sQuote("columns"), "is empty."))
    }
    if (length(columns) != 0) {
      if (!is.integer(columns) | anyNA(columns) | !isPositive(columns, strict = TRUE))
        errors <- c(errors, paste(sQuote("columns"), "must be a vector of strictly positive integers."))
      if (length(rows) == 0)
        errors <- c(errors, paste(sQuote("columns"), "is empty."))
    }
    if (length(method) != 0) {
      if (length(method) != 1 | !is.character(method)) {
        errors <- c(errors, paste(sQuote("method"), "must be a single character string."))
      }
    }
    if (length(rows) != 0 & length(columns) != 0 & length(method) == 0) {
      errors <- c(errors, paste(sQuote("method"), "is missing."))
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
        errors <- c(errors, "Numeric values are expected.")
      if (anyNA(data))
        errors <- c(errors, "NA values were detected.")
      if (any(is.infinite(data)))
        errors <- c(errors, "Infinite numbers were detected.")
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
        errors <- c(errors, "Whole numbers are expected.")
      if (isBinary(data))
        errors <- c(errors, "You should consider using an incidence matrix.")
      if (!isPositive(data))
        errors <- c(errors, "Positive values are expected.")
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
        errors <- c(errors, "Constant row sums are expected.")
      if (isBinary(data))
        errors <- c(errors, "You should consider using an incidence matrix.")
      if (!isPositive(data))
        errors <- c(errors, "Positive values are expected.")
      k <- length(totals)
      if (k != nrow(data))
        errors <- c(errors, paste(sQuote("totals"), "should be of length",
                                  nrow(data), "not", k))
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
        errors <- c(errors, "A square matrix is expected.")
      if (!isSymmetric(data))
        errors <- c(errors, "A symmetric matrix is expected.")
      if (!identical(rownames(data), colnames(data)))
        errors <- c(errors, "Rows and columns must have the same names.")
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
    method <- object@method

    # Check data
    if (length(data) != 0) {
      if (!isSquare(data))
        errors <- c(errors, "A square matrix is expected.")
      if (!isSymmetric(data))
        errors <- c(errors, "A symmetric matrix is expected.")
      if (!identical(rownames(data), colnames(data)))
        errors <- c(errors, "Rows and columns must have the same names.")
      if (length(method) != 1)
        errors <- c(errors, paste(sQuote("method"), "must be a single character string."))
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
        errors <- c("Logical values are expected.")
      if (anyNA(data))
        errors <- c(errors, "Missing values were detected.")
    }

    # Return errors, if any
    if (length(errors) != 0) {
      stop(paste(class(object), errors, sep = ": ", collapse = "\n  "))
    } else {
      return(TRUE)
    }
  }
)

