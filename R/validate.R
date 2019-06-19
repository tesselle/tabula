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
    id <- object@id
    counts <- object@counts
    level <- object@level
    # model <- object@model
    rows <- object@rows
    columns <- object@columns
    accumulation <- object@accumulation

    errors <- list(
      # Check id
      id = c(
        catchConditions(checkUUID(id))
      ),
      counts = c(
        catchConditions(checkType(counts, expected = "numeric")),
        catchConditions(checkMissing(counts)),
        catchConditions(checkInfinite(counts))
      ),
      level = c(
        catchConditions(checkScalar(level, expected = "numeric")),
        catchConditions(checkMissing(level)),
        catchConditions(checkInfinite(level))
      ),
      rows = c(
        catchConditions(checkType(rows, expected = "numeric")),
        catchConditions(checkMissing(rows)),
        catchConditions(checkInfinite(rows)),
        catchConditions(checkColnames(rows, expected = c("date", "lower",
                                                         "upper", "error")))
      ),
      columns = c(
        catchConditions(checkType(columns, expected = "numeric")),
        catchConditions(checkMissing(columns)),
        catchConditions(checkInfinite(columns)),
        catchConditions(checkColnames(columns, expected = c("date", "lower",
                                                            "upper", "error")))
      ),
      accumulation = c(
        catchConditions(checkType(accumulation, expected = "numeric")),
        catchConditions(checkMissing(accumulation)),
        catchConditions(checkInfinite(accumulation)),
        catchConditions(checkColnames(accumulation,
                                      expected = c("date", "error")))
      )
    )

    # Return errors if any
    formatErrors(object, errors)
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
        catchConditions(checkNumbers(rows, "positive", strict = TRUE))
      ),
      columns = c(
        catchConditions(checkMissing(columns)),
        catchConditions(checkNumbers(columns, "positive", strict = TRUE))
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
        catchConditions(checkType(dates, expected = "numeric")),
        catchConditions(checkInfinite(dates)),
        catchConditions(checkColnames(dates, expected = c("value", "error")))
      ),
      coordinates = c(
        catchConditions(checkType(coordinates, expected = "numeric")),
        catchConditions(checkInfinite(coordinates)),
        catchConditions(checkColnames(coordinates, expected = c("x", "y", "z")))
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
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")
    dates <- object@dates
    coordinates <- object@coordinates
    n <- nrow(data)

    errors <- list()
    if (length(dates) != 0 && nrow(dates) > 0) {
      # Check dates
      errors[["dates"]] <- c(
        catchConditions(checkLength(dates, expected = n * 2))
      )
    }
    if (length(coordinates) != 0 && nrow(coordinates) > 0) {
      # Check coordinates
      errors[["coordinates"]] <- c(
        catchConditions(checkLength(coordinates, expected = n * 3))
      )
    }

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

    errors <- list(
      # Check data
      data = c(
        catchConditions(checkNumbers(data, "positive", strict = FALSE)),
        catchConditions(checkNumbers(data, "whole"))
      )
    )
    # Messages
    # TODO: warning instead of message?
    if (all(isBinary(data)))
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
        catchConditions(checkNumbers(data, "positive", strict = FALSE)),
        catchConditions(checkConstant(data))
      ),
      # Check totals
      totals = c(
        catchConditions(checkLength(totals, expected = n))
      )
    )

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
# setValidity(
#   Class = "IncidenceMatrix",
#   method = function(object) {
#
#   }
# )
