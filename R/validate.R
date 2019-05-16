# CLASSES VALIDATION
#' @include AllClasses.R utilities.R
NULL

# BootCA =======================================================================
setValidity(
  Class = "BootCA",
  method = function(object) {
    # Get data
    rows <- object@rows
    columns <- object@columns
    lengths <- object@lengths
    cutoff <- object@cutoff
    keep <- object@keep

    errors <- list(
      rows = c(
        unlist(mapply(
          FUN = checkClass, rows, list("factor", "numeric", "numeric")
        )),
        unlist(sapply(X = rows, FUN = checkIfNA)),
        unlist(sapply(X = rows[c(2, 3)], FUN = checkIfNaN)),
        unlist(sapply(X = rows[c(2, 3)], FUN = checkIfInf)),
        checkLength(rows, expected = 3),
        checkLengths(rows),
        checkNames(rows, expected = c("id", "x", "y"))
      ),
      columns = c(
        unlist(mapply(
          FUN = checkClass, columns, list("factor", "numeric", "numeric")
        )),
        unlist(sapply(X = columns, FUN = checkIfNA)),
        unlist(sapply(X = columns[c(2, 3)], FUN = checkIfNaN)),
        unlist(sapply(X = columns[c(2, 3)], FUN = checkIfInf)),
        checkLength(columns, expected = 3),
        checkLengths(columns),
        checkNames(columns, expected = c("id", "x", "y"))
      ),
      lengths = c(
        unlist(sapply(X = lengths, FUN = checkClass, expected = "numeric")),
        unlist(sapply(X = lengths, FUN = checkIfNA)),
        unlist(sapply(X = lengths, FUN = checkIfNaN)),
        unlist(sapply(X = lengths, FUN = checkIfInf)),
        checkLength(lengths, expected = 2),
        checkNames(lengths[[1]]),
        checkNames(lengths[[2]])
      ),
      cutoff = c(
        checkLength(cutoff, expected = 2),
        checkIfNA(cutoff),
        checkIfNaN(cutoff),
        checkIfInf(cutoff)
      ),
      keep = c(
        unlist(sapply(X = keep, FUN = checkClass, expected = "integer")),
        unlist(sapply(X = keep, FUN = checkIfNA)),
        unlist(sapply(X = keep, FUN = checkIfNaN)),
        unlist(sapply(X = keep, FUN = checkIfInf)),
        checkLength(keep, expected = 2)
      )
    )
    # Return errors if any
    returnSlotErrors(object, errors)
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
      # Check id
      id = c(
        checkLength(id, expected = 1),
        checkIfNA(id),
        checkIfUUID(id)
      ),
      rows = c(
        checkIfNA(rows),
        checkIfPositive(rows, strict = TRUE, na.rm = TRUE)
      ),
      columns = c(
        checkIfNA(columns),
        checkIfPositive(columns, strict = TRUE, na.rm = TRUE)
      ),
      method = c(
        checkIfNA(method),
        checkLength(method, expected = 1)
      )
    )

    # Return errors if any
    returnSlotErrors(object, errors)
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
        unlist(sapply(X = dates, FUN = checkType, expected = "numeric")),
        unlist(sapply(X = dates, FUN = checkIfInf)),
        checkLength(dates, expected = 2),
        checkLengths(dates),
        checkNames(dates, expected = c("value", "error"))
      ),
      coordinates = c(
        unlist(sapply(X = coordinates, FUN = checkType, expected = "numeric")),
        unlist(sapply(X = coordinates, FUN = checkIfInf)),
        checkLength(coordinates, expected = 3),
        checkLengths(coordinates),
        checkNames(coordinates, expected = c("x", "y", "z"))
      ),
      epsg = c(
        checkLength(epsg, expected = 1),
        checkIfNA(epsg)
      )
    )

    # Return errors if any
    returnSlotErrors(object, errors)
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
        checkIfInf(data),
        checkIfNA(data),
        checkIfNaN(data)
      ),
      # Check id
      id = c(
        checkLength(id, expected = 1),
        checkIfNA(id),
        checkIfUUID(id)
      )
    )
    # Return errors if any
    returnSlotErrors(object, errors)
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
      data = checkType(data, expected = "numeric")
    )

    # Return errors if any
    returnSlotErrors(object, errors)
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

    errors <- list(
      # Check data
      data = c(
        checkIfPositive(data, strict = FALSE, na.rm = TRUE),
        checkIfWholeNumber(data),
        checkIfBinaryMatrix(data)
      ),
      # Check dates
      dates = c(
        checkLengths(dates, expected = nrow(data))
      ),
      # Check coordinates
      coordinates = c(
        checkLengths(coordinates, expected = nrow(data))
      )
    )

    # Return errors, if any
    returnSlotErrors(object, errors)
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

    errors <- list(
      # Check data
      data = c(
        checkIfPositive(data, strict = FALSE, na.rm = TRUE),
        checkIfConstantSum(data)
      ),
      # Check totals
      totals = c(
        checkLength(totals, expected = nrow(data))
      ),
      # Check dates
      dates = c(
        checkLengths(dates, expected = nrow(data))
      ),
      # Check coordinates
      coordinates = c(
        checkLengths(coordinates, expected = nrow(data))
      )
    )

    # Return errors, if any
    returnSlotErrors(object, errors)
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
        checkIfSymmetric(data)
      )
    )

    # Return errors, if any
    returnSlotErrors(object, errors)
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
        checkIfSymmetric(data)
      ),
      # Check method
      method = c(
        checkLength(method, expected = 1)
      )
    )

    # Return errors, if any
    returnSlotErrors(object, errors)
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
      data = checkType(data, expected = "logical")
    )

    # Return errors if any
    returnSlotErrors(object, errors)
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

    errors <- list(
      # Check dates
      dates = c(
        checkLengths(dates, expected = nrow(data))
      ),
      # Check coordinates
      coordinates = c(
        checkLengths(coordinates, expected = nrow(data))
      )
    )

    # Return errors if any
    returnSlotErrors(object, errors)
  }
)
