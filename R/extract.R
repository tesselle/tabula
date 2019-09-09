# ACCESSORS
#' @include AllClasses.R
NULL

# BootCA =======================================================================
#' @export
#' @rdname subset
#' @aliases [,BootCA-method
setMethod(
  f = "[",
  signature = "BootCA",
  definition = function(x, i, j, drop = TRUE) {
    i <- match.arg(i, choices = c("rows", "columns"), several.ok = FALSE)
    data <- as.data.frame(methods::slot(x, i))

    if (missing(j)) {
      j <- seq_len(nrow(data))
    } else {
      if (is.null(j)) j <- seq_len(nrow(data))
      if (is.character(j) | is.factor(j)) j <- which(data$id %in% j)
      if (is.numeric(j)) j <- as.integer(j)
    }
    data <- data[j, , drop = drop]
    data
  }
)
# @export
# @rdname subset
# @aliases [,SpaceTime-method
# setMethod(
#   f = "[",
#   signature = "SpaceTime",
#   definition = function(x, i, j, drop = TRUE) {
#     i <- match.arg(i, choices = c("dates", "coordinates"), several.ok = FALSE)
#     data <- as.data.frame(methods::slot(x, i))
#
#     if (missing(j)) {
#       j <- 1:nrow(data)
#     } else {
#       if (is.null(j)) j <- 1:nrow(data)
#       if (is.character(j) | is.factor(j)) j <- which(data$id %in% j)
#       if (is.numeric(j)) j <- as.integer(j)
#     }
#     data <- data[j, , drop = drop]
#     data
#   }
# )
#' @export
#' @rdname subset
#' @aliases [,DateModel-method
setMethod(
  f = "[",
  signature = "DateModel",
  definition = function(x, i, j, drop = TRUE) {
    i <- match.arg(i, choices = c("counts", "dates",
                                  "rows", "columns", "accumulation",
                                  "jackknife", "bootstrap"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)

    m <- nrow(data)
    if (m != 0) {
      if (missing(j)) {
        j <- seq_len(m)
      } else {
        if (is.null(j)) j <- seq_along(m)
        if (is.character(j) | is.factor(j)) j <- which(data$id %in% j)
        if (is.numeric(j)) j <- as.integer(j)
      }
      data <- data[j, , drop = drop]
    }
    data
  }
)

# ------------------------------------------------------------------------------
#' Extract Parts of an Object
#'
#' @inheritParams subset
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
extractSlot <- function(x, i) {
  class_name <- class(x)
  i <- match.arg(i, choices = methods::slotNames(class_name),
                 several.ok = FALSE)
  data <- methods::slot(x, i)
  data
}

#' @export
#' @rdname subset
#' @aliases [[,DateModel-method
setMethod(
  f = "[[",
  signature = "DateModel",
  definition = extractSlot
)
#' @export
#' @rdname subset
#' @aliases [[,BootCA-method
setMethod(
  f = "[[",
  signature = "BootCA",
  definition = function(x, i) {
    data <- extractSlot(x, i)
    if (i %in% c("rows", "columns"))
      data <- as.data.frame(data)
    data
  }
)
#' @export
#' @rdname subset
#' @aliases [[,PermutationOrder-method
setMethod(
  f = "[[",
  signature = "PermutationOrder",
  definition = extractSlot
)
#' @export
#' @rdname subset
#' @aliases [[,SpaceTime-method
setMethod(
  f = "[[",
  signature = "SpaceTime",
  definition = function(x, i) {
    data <- extractSlot(x, i)
    if (i %in% c("dates", "coordinates"))
      data <- as.data.frame(data)
    data
  }
)
#' @export
#' @rdname subset
#' @aliases [[,CountMatrix-method
setMethod(
  f = "[[",
  signature = "CountMatrix",
  definition = extractSlot
)
#' @export
#' @rdname subset
#' @aliases [[,FrequencyMatrix-method
setMethod(
  f = "[[",
  signature = "FrequencyMatrix",
  definition = extractSlot
)
#' @export
#' @rdname subset
#' @aliases [[,IncidenceMatrix-method
setMethod(
  f = "[[",
  signature = "IncidenceMatrix",
  definition = extractSlot
)
#' @export
#' @rdname subset
#' @aliases [[,OccurrenceMatrix-method
setMethod(
  f = "[[",
  signature = "OccurrenceMatrix",
  definition = extractSlot
)
#' @export
#' @rdname subset
#' @aliases [[,SimilarityMatrix-method
setMethod(
  f = "[[",
  signature = "SimilarityMatrix",
  definition = extractSlot
)

# Getters ======================================================================
#' @export
#' @rdname access
#' @aliases get_coordinates,AbundanceMatrix-method
setMethod(
  f = "get_coordinates",
  signature = "AbundanceMatrix",
  definition = function(object) {
    coords <- object@coordinates
    coords <- as.list(as.data.frame(coords))
    attr(coords, "epsg") <- object@epsg
    coords
  }
)

#' @export
#' @rdname access
#' @aliases get_dates,AbundanceMatrix-method
setMethod(
  f = "get_dates",
  signature = "AbundanceMatrix",
  definition = function(object) {
    dates <- object@dates
    as.list(as.data.frame(dates))
  }
)

#' @export
#' @rdname access
#' @aliases get_epsg,AbundanceMatrix-method
setMethod("get_epsg", "AbundanceMatrix", function(object) object@epsg)

#' @export
#' @rdname access
#' @aliases get_id,ANY-method
setMethod("get_id", "ANY", function(object) object@id)

#' @export
#' @rdname access
#' @aliases get_totals,FrequencyMatrix-method
setMethod("get_totals", "FrequencyMatrix", function(object) object@totals)

# Setters ======================================================================
# Totals -----------------------------------------------------------------------
#' @export
#' @rdname access
#' @aliases set_totals,FrequencyMatrix-method
setMethod(
  f = "set_totals<-",
  signature = "FrequencyMatrix",
  definition = function(object, value) {
    object@totals <- value
    methods::validObject(object)
    object
  }
)

# Dates ------------------------------------------------------------------------
makeDates <- function(value) {
  if (is.matrix(value) | is.data.frame(value)) {
    value <- data.matrix(value)
    if (ncol(value) >= 2) {
      if (all(c("value", "error") %in% colnames(value))) {
        x <- value[, "value"]
        y <- value[, "error"]
      } else {
        x <- value[, 1]
        y <- value[, 2]
      }
    } else {
      stop("`value` should have at least 2 columns.", call. = FALSE)
    }
  } else if(is.list(value)) {
    if (all(c("value", "error") %in% names(value))) {
      x <- value[["value"]]
      y <- value[["error"]]
    } else {
      stop("`value` is a list, ",
           "but does not have components 'value' and 'error'.",
           call. = FALSE)
    }
  } else if (is.numeric(value) || is.integer(value) || is.character(value)) {
    x <- value
    y <- rep_len(0, length.out = length(x))
    if (getOption("verbose")) message("Errors are missing, NA generated.")
  } else if (is.null(value)) {
    x <- y <- numeric(0)
  } else {
    stop("`value` must be a numeric, integer or character vector, ",
         "a list, a matrix, a data frame or NULL.", call. = FALSE)
  }
  # If `x` is a character vector, try to convert from roman numbers
  if (is.character(x)) {
   x <- as.numeric(utils::as.roman(x))
  }
  # Replace NA with zeros
  y[is.na(y)] <- 0
  cbind(value = x, error = y)
}

#' @export
#' @rdname access
#' @aliases set_dates,AbundanceMatrix-method
setMethod(
  f = "set_dates<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    value <- makeDates(value)
    rows_value <- rownames(value)
    rows_object <- rownames(object)
    i <- nrow(object)
    if (all(dim(value) == c(i, 2)) || nrow(value) == 0) {
      # Same dimensions or unset
      B <- value
      k <- nrow(B)
    } else if (!is.null(rows_value) && !is.null(rows_object)) {
      # Match by names
      index <- match(rows_value, rows_object)
      no_match <- detect(f = is.na, x = index)
      if (any(no_match)) {
        warning(ngettext(sum(no_match),
                         "The following date do not match and was skiped:\n",
                         "The following dates do not match and were skiped:\n"),
                paste0("* ", rows_value[no_match], collapse = "\n"),
                call. = FALSE)
      }
      index_clean <- compact(f = is.na, x = index)
      B <- matrix(NA_real_, nrow = i, ncol = 2,
                  dimnames = list(rows_object, c("value", "error")))
      B[index_clean, ] <- value[!no_match]
      k <- sum(!no_match)
    } else {
      stop("Cannot interpret `value` in a suitable way.", call. = FALSE)
    }

    object@dates <- B
    methods::validObject(object)
    if (getOption("verbose")) {
      message(sprintf(ngettext(k, "%d date was set.", "%d dates were set."), k))
    }
    object
  }
)

# Coordinates ------------------------------------------------------------------
makeXYZ <- function(value) {
  if (is.matrix(value) | is.data.frame(value)) {
    value <- data.matrix(value)
    if (ncol(value) >= 2) {
      if (all(c("x", "y") %in% colnames(value))) {
        x <- value[, "x"]
        y <- value[, "y"]
      } else {
        x <- value[, 1]
        y <- value[, 2]
      }
    } else {
      stop("`value` should have at least 2 columns.", call. = FALSE)
    }
    if (ncol(value) >= 3) {
      if ("z" %in% colnames(value)) {
        z <- value[, "z"]
      } else {
        z <- value[, 3]
      }
    } else {
      z <- rep_len(x = NA_real_, length.out = nrow(value))
      if (getOption("verbose")) message("'z' is missing, NA generated.")
    }
  } else if(is.list(value)) {
    if (all(c("x", "y") %in% names(value))) {
      x <- value[["x"]]
      y <- value[["y"]]
    } else {
      stop("`value` is a list, but does not have components 'x' and 'y'.",
           call. = FALSE)
    }
    if ("z" %in% names(value)) {
      z <- value[["z"]]
    } else {
      z <- rep_len(x = NA_real_, length.out = length(x))
      if (getOption("verbose")) message("'z' is missing, NA generated.")
    }
  } else {
    stop("`value` must be a list, a matrix or a data frame.",
         call. = FALSE)
  }
  cbind(x = x, y = y, z = z)
}

#' @export
#' @rdname access
#' @aliases set_coordinates,AbundanceMatrix-method
setMethod(
  f = "set_coordinates<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    object@coordinates <- makeXYZ(value)
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname access
#' @aliases set_epsg,AbundanceMatrix-method
setMethod(
  f = "set_epsg<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    if (!(is.numeric(value) | is.integer(value)) || length(value) != 1)
      stop("`value` should be a length-one numeric or integer vector.",
           call. = FALSE)
    object@epsg <- as.integer(value[1L])
    methods::validObject(object)
    object
  }
)
