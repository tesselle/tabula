# CLASSES VALIDATION
#' @include AllClasses.R utilities.R check.R
NULL

# BootCA =======================================================================
setValidity(
  Class = "BootCA",
  method = function(object) {
    # Get data
    id <- object@id
    rows <- object@rows
    columns <- object@columns
    lengths <- object@lengths
    cutoff <- object@cutoff
    keep <- object@keep

    errors <- list(
      id = c(
        catchConditions(checkUUID(id))
      ),
      rows = c(
        unlist(mapply(
          FUN = function(x, expected) catchConditions(checkType(x, expected)),
          rows, list("integer", "numeric", "numeric")
        )),
        unlist(lapply(
          X = rows,
          FUN = function(x) {
            c(catchConditions(checkMissing(x)),
              catchConditions(checkInfinite(x)))
          }
        )),
        catchConditions(checkLength(rows, expected = 3)),
        catchConditions(checkLengths(rows)),
        catchConditions(checkNames(rows, expected = c("id", "x", "y")))
      ),
      columns = c(
        unlist(mapply(
          FUN = function(x, expected) catchConditions(checkType(x, expected)),
          columns, list("integer", "numeric", "numeric")
        )),
        unlist(lapply(
          X = columns,
          FUN = function(x) {
            c(catchConditions(checkMissing(x)),
              catchConditions(checkInfinite(x)))
          }
        )),
        catchConditions(checkLength(columns, expected = 3)),
        catchConditions(checkLengths(columns)),
        catchConditions(checkNames(columns, expected = c("id", "x", "y")))
      ),
      lengths = c(
        unlist(lapply(
          X = lengths,
          FUN = function(x) {
            c(catchConditions(checkType(x, expected = "numeric")),
              # catchConditions(checkNames(x)),
              catchConditions(checkMissing(x)),
              catchConditions(checkInfinite(x)))
          }
        )),
        catchConditions(checkLength(lengths, expected = 2))
      ),
      cutoff = c(
        catchConditions(checkLength(cutoff, expected = 2)),
        catchConditions(checkMissing(cutoff)),
        catchConditions(checkInfinite(cutoff))
      ),
      keep = c(
        unlist(lapply(
          X = keep,
          FUN = function(x) {
            c(catchConditions(checkType(x, expected = "integer")),
              catchConditions(checkMissing(x)))
          }
        )),
        catchConditions(checkLength(keep, expected = 2))
      )
    )
    # Return errors if any
    formatErrors(object, errors)
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
      printErrors(object, errors, call = FALSE)
    } else {
      return(TRUE)
    }
  }
)

# PermutationOrder =============================================================
setValidity(
  Class = "PermutationOrder",
  method = function(object) {
    # Get data
    id <- object@id
    rows <- object@rows
    columns <- object@columns
    method <- object@method

    errors <- list(
      id = c(
        catchConditions(checkUUID(id))
      ),
      rows = c(
        catchConditions(checkMissing(rows)),
        catchConditions(checkNumbers(rows, "positive",
                                     strict = TRUE, na.rm = TRUE))
      ),
      columns = c(
        catchConditions(checkMissing(columns)),
        catchConditions(checkNumbers(columns, "positive",
                                     strict = TRUE, na.rm = TRUE))
      ),
      method = c(
        checkScalar(method, expected = "character"),
        checkMissing(method)
      )
    )

    # Return errors if any
    formatErrors(object, errors)
  }
)

# SpaceTime ====================================================================
setValidity(
  Class = "SpaceTime",
  method = function(object) {
    # Get data
    dates <- object@dates
    coordinates <- object@coordinates
    epsg <- object@epsg

    # Check dates
    errors <- list(
      dates = c(
        unlist(lapply(
          X = dates,
          FUN = function(x) {
            c(catchConditions(checkType(x, expected = "numeric")),
              catchConditions(checkInfinite(x)))
          }
        )),
        catchConditions(checkLength(dates, expected = 2)),
        catchConditions(checkLengths(dates)),
        catchConditions(checkNames(dates, expected = c("value", "error")))
      ),
      coordinates = c(
        unlist(lapply(
          X = coordinates,
          FUN = function(x) {
            c(catchConditions(checkType(x, expected = "numeric")),
              catchConditions(checkInfinite(x)))
          }
        )),
        catchConditions(checkLength(coordinates, expected = 3)),
        catchConditions(checkLengths(coordinates)),
        catchConditions(checkNames(coordinates, expected = c("x", "y", "z")))
      ),
      epsg = c(
        catchConditions(checkScalar(epsg, expected = "integer")),
        catchConditions(checkMissing(epsg))
      )
    )

    # Return errors if any
    formatErrors(object, errors)
  }
)
# Matrix =======================================================================
setValidity(
  Class = "Matrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")
    id <- object@id

    errors <- list(
      # Check data
      data = c(
        catchConditions(checkMissing(data)),
        catchConditions(checkInfinite(data))
      ),
      # Check id
      id = c(
        catchConditions(checkUUID(id))
      )
    )

    # Return errors if any
    formatErrors(object, errors)
  }
)

# NumericMatrix ================================================================
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")

    errors <- list(
      # Check data
      data = catchConditions(checkType(data, "numeric"))
    )

    # Return errors if any
    formatErrors(object, errors)
  }
)

## CountMatrix -----------------------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    dates <- object@dates
    coordinates <- object@coordinates
    n <- nrow(data)

    errors <- list(
      # Check data
      data = c(
        catchConditions(checkNumbers(data, "positive", strict = FALSE,
                                     na.rm = TRUE)),
        catchConditions(checkNumbers(data, "whole"))
      )
    )
    if (!all(lengths(dates) == 0)) {
      # Check dates
      errors[["dates"]] <- c(
        catchConditions(checkLengths(dates, expected = n))
      )
    }
    if (!all(lengths(coordinates) == 0)) {
      # Check coordinates
      errors["coordinates"] <- c(
        catchConditions(checkLengths(coordinates, expected = n))
      )
    }
    # Messages
    # TODO: warning instead of message?
    if (isBinary(data))
      message("Your matrix contains only 0s and 1s.\n",
              "You should consider using an incidence matrix instead.")

    # Return errors, if any
    formatErrors(object, errors)
  }
)

## FrequencyMatrix -------------------------------------------------------------
setValidity(
  Class = "FrequencyMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    totals <- object@totals
    dates <- object@dates
    coordinates <- object@coordinates
    n <- nrow(data)

    errors <- list(
      # Check data
      data = c(
        catchConditions(checkNumbers(data, "positive", strict = FALSE,
                                     na.rm = TRUE)),
        catchConditions(checkConstant(data))
      ),
      # Check totals
      totals = c(
        catchConditions(checkLength(totals, expected = n))
      )
    )
    if (!all(lengths(dates) == 0)) {
      # Check dates
      errors[["dates"]] <- c(
        catchConditions(checkLengths(dates, expected = n))
      )
    }
    if (!all(lengths(coordinates) == 0)) {
      # Check coordinates
      errors["coordinates"] <- c(
        catchConditions(checkLengths(coordinates, expected = n))
      )
    }

    # Return errors, if any
    formatErrors(object, errors)
  }
)

## OccurrenceMatrix ------------------------------------------------------------
setValidity(
  Class = "OccurrenceMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    errors <- list(
      # Check data
      data = c(
        catchConditions(checkMatrix(data, expected = "symmetric"))
      )
    )

    # Return errors, if any
    formatErrors(object, errors)
  }
)

## SimilarityMatrix ------------------------------------------------------------
setValidity(
  Class = "SimilarityMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    method <- object@method

    errors <- list(
      # Check data
      data = c(
        catchConditions(checkMatrix(data, expected = "symmetric"))
      ),
      # Check method
      method = c(
        catchConditions(checkScalar(method, expected = "character")),
        catchConditions(checkMissing(method))
      )
    )

    # Return errors, if any
    formatErrors(object, errors)
  }
)

# LogicalMatrix ================================================================
setValidity(
  Class = "LogicalMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")

    errors <- list(
      # Check data
      data = catchConditions(checkType(data, expected = "logical"))
    )

    # Return errors if any
    formatErrors(object, errors)
  }
)

## IncidenceMatrix -------------------------------------------------------------
setValidity(
  Class = "IncidenceMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")
    dates <- object@dates
    coordinates <- object@coordinates
    n <- nrow(data)

    errors <- list()
    if (!all(lengths(dates) == 0)) {
      # Check dates
      errors[["dates"]] <- c(
        catchConditions(checkLengths(dates, expected = n))
      )
    }
    if (!all(lengths(coordinates) == 0)) {
      # Check coordinates
      errors["coordinates"] <- c(
        catchConditions(checkLengths(coordinates, expected = n))
      )
    }

    # Return errors if any
    formatErrors(object, errors)
  }
)
