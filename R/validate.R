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
        catch_conditions(check_uuid(id))
      ),
      rows = c(
        unlist(mapply(
          FUN = function(x, expected) catch_conditions(check_type(x, expected)),
          rows, list("integer", "numeric", "numeric")
        )),
        unlist(lapply(
          X = rows,
          FUN = function(x) {
            c(catch_conditions(check_missing(x)),
              catch_conditions(check_infinite(x)))
          }
        )),
        catch_conditions(check_length(rows, expected = 3)),
        catch_conditions(check_lengths(rows)),
        catch_conditions(check_names(rows, expected = c("id", "x", "y")))
      ),
      columns = c(
        unlist(mapply(
          FUN = function(x, expected) catch_conditions(check_type(x, expected)),
          columns, list("integer", "numeric", "numeric")
        )),
        unlist(lapply(
          X = columns,
          FUN = function(x) {
            c(catch_conditions(check_missing(x)),
              catch_conditions(check_infinite(x)))
          }
        )),
        catch_conditions(check_length(columns, expected = 3)),
        catch_conditions(check_lengths(columns)),
        catch_conditions(check_names(columns, expected = c("id", "x", "y")))
      ),
      lengths = c(
        unlist(lapply(
          X = lengths,
          FUN = function(x) {
            c(catch_conditions(check_type(x, expected = "numeric")),
              # catch_conditions(check_names(x)),
              catch_conditions(check_missing(x)),
              catch_conditions(check_infinite(x)))
          }
        )),
        catch_conditions(check_length(lengths, expected = 2))
      ),
      cutoff = c(
        catch_conditions(check_length(cutoff, expected = 2)),
        catch_conditions(check_missing(cutoff)),
        catch_conditions(check_infinite(cutoff))
      ),
      keep = c(
        unlist(lapply(
          X = keep,
          FUN = function(x) {
            c(catch_conditions(check_type(x, expected = "integer")),
              catch_conditions(check_missing(x)))
          }
        )),
        catch_conditions(check_length(keep, expected = 2))
      )
    )
    # Return errors if any
    throw_error_class(object, errors)
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
      id = c(
        catch_conditions(check_uuid(id))
      ),
      counts = c(
        catch_conditions(check_type(counts, expected = "numeric")),
        catch_conditions(check_missing(counts)),
        catch_conditions(check_infinite(counts))
      ),
      level = c(
        catch_conditions(check_scalar(level, expected = "numeric")),
        catch_conditions(check_missing(level)),
        catch_conditions(check_infinite(level))
      ),
      rows = c(
        catch_conditions(check_type(rows, expected = "numeric")),
        catch_conditions(check_missing(rows)),
        catch_conditions(check_infinite(rows)),
        catch_conditions(check_names(rows, expected = c("date", "lower", "upper", "error"),
                                     margin = 2))
      ),
      columns = c(
        catch_conditions(check_type(columns, expected = "numeric")),
        catch_conditions(check_missing(columns)),
        catch_conditions(check_infinite(columns)),
        catch_conditions(check_names(columns, expected = c("date", "lower", "upper", "error"),
                                     margin = 2))
      ),
      accumulation = c(
        catch_conditions(check_type(accumulation, expected = "numeric")),
        catch_conditions(check_missing(accumulation)),
        catch_conditions(check_infinite(accumulation)),
        catch_conditions(check_names(accumulation, expected = c("date", "error"),
                                     margin = 2))
      )
    )

    # Return errors if any
    throw_error_class(object, errors)
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
        catch_conditions(check_uuid(id))
      ),
      rows = c(
        catch_conditions(check_missing(rows)),
        catch_conditions(check_numbers(rows, expected = "positive",
                                       strict = TRUE))
      ),
      columns = c(
        catch_conditions(check_missing(columns)),
        catch_conditions(check_numbers(columns, expected = "positive",
                                       strict = TRUE))
      ),
      method = c(
        catch_conditions(check_scalar(method, expected = "character")),
        catch_conditions(check_missing(method))
      )
    )

    # Return errors if any
    throw_error_class(object, errors)
  }
)
