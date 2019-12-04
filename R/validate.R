# CLASSES VALIDATION
#' @include AllClasses.R utilities.R
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
      # Check id
      codex:::catch_conditions(codex:::check_uuid(id)),
      # Check rows
      codex:::catch_conditions(codex:::check_length(rows, 3)),
      codex:::catch_conditions(codex:::check_lengths(rows)),
      codex:::catch_conditions(codex:::check_names(rows, c("id", "x", "y"))),
      codex:::catch_conditions(codex:::check_type(rows[[1]], "integer")),
      codex:::catch_conditions(codex:::check_type(rows[[2]], "numeric")),
      codex:::catch_conditions(codex:::check_type(rows[[3]], "numeric")),
      unlist(lapply(
        X = rows,
        FUN = function(x) {
          c(codex:::catch_conditions(codex:::check_missing(x)),
            codex:::catch_conditions(codex:::check_infinite(x)))
        }
      )),
      # Check columns
      codex:::catch_conditions(codex:::check_length(columns, 3)),
      codex:::catch_conditions(codex:::check_lengths(columns)),
      codex:::catch_conditions(codex:::check_names(columns, c("id", "x", "y"))),
      codex:::catch_conditions(codex:::check_type(columns[[1]], "integer")),
      codex:::catch_conditions(codex:::check_type(columns[[2]], "numeric")),
      codex:::catch_conditions(codex:::check_type(columns[[3]], "numeric")),
      unlist(lapply(
        X = columns,
        FUN = function(x) {
          c(codex:::catch_conditions(codex:::check_missing(x)),
            codex:::catch_conditions(codex:::check_infinite(x)))
        }
      )),
      # Check lenghts
      codex:::catch_conditions(codex:::check_length(lengths, expected = 2)),
      codex:::catch_conditions(codex:::check_type(lengths[[1]], "numeric")),
      codex:::catch_conditions(codex:::check_type(lengths[[2]], "numeric")),
      unlist(lapply(
        X = lengths,
        FUN = function(x) {
          c(# catch_conditions(check_names(x)),
            codex:::catch_conditions(codex:::check_missing(x)),
            codex:::catch_conditions(codex:::check_infinite(x)))
        }
      )),
      # Check cutoff
      codex:::catch_conditions(codex:::check_length(cutoff, 2)),
      codex:::catch_conditions(codex:::check_missing(cutoff)),
      codex:::catch_conditions(codex:::check_infinite(cutoff)),
      # Check keep
      unlist(lapply(
        X = keep,
        FUN = function(x) {
          c(codex:::catch_conditions(codex:::check_type(x, expected = "integer")),
            codex:::catch_conditions(codex:::check_missing(x)))
        }
      )),
      codex:::catch_conditions(codex:::check_length(keep, 2))
    )

    # Return errors if any
    codex:::check_class(object, errors)
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
      codex:::catch_conditions(codex:::check_uuid(id)),
      # Check counts
      codex:::catch_conditions(codex:::check_type(counts, "numeric")),
      codex:::catch_conditions(codex:::check_missing(counts)),
      codex:::catch_conditions(codex:::check_infinite(counts)),
      # Check level
      codex:::catch_conditions(codex:::check_scalar(level, "numeric")),
      codex:::catch_conditions(codex:::check_missing(level)),
      codex:::catch_conditions(codex:::check_infinite(level)),
      # Check rows
      codex:::catch_conditions(codex:::check_type(rows, "numeric")),
      codex:::catch_conditions(codex:::check_missing(rows)),
      codex:::catch_conditions(codex:::check_infinite(rows)),
      codex:::catch_conditions(codex:::check_names(rows, c("date", "lower", "upper", "error"),
                                                   margin = 2)),
      # Check columns
      codex:::catch_conditions(codex:::check_type(columns, "numeric")),
      codex:::catch_conditions(codex:::check_missing(columns)),
      codex:::catch_conditions(codex:::check_infinite(columns)),
      codex:::catch_conditions(codex:::check_names(columns, c("date", "lower", "upper", "error"),
                                                   margin = 2)),
      # Check accumulation
      codex:::catch_conditions(codex:::check_type(accumulation, "numeric")),
      codex:::catch_conditions(codex:::check_missing(accumulation)),
      codex:::catch_conditions(codex:::check_infinite(accumulation)),
      codex:::catch_conditions(codex:::check_names(accumulation, c("date", "error"),
                                                   margin = 2))
    )

    # Return errors if any
    codex:::check_class(object, errors)
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
      codex:::catch_conditions(codex:::check_uuid(id)),
      # Check rows
      codex:::catch_conditions(codex:::check_missing(rows)),
      codex:::catch_conditions(codex:::check_numbers(rows, "positive",
                                                     strict = TRUE)),
      # Check columns
      codex:::catch_conditions(codex:::check_missing(columns)),
      codex:::catch_conditions(codex:::check_numbers(columns, "positive",
                                                     strict = TRUE)),
      # Check method
      codex:::catch_conditions(codex:::check_scalar(method, "character")),
      codex:::catch_conditions(codex:::check_missing(method))
    )

    # Return errors if any
    codex:::check_class(object, errors)
  }
)
