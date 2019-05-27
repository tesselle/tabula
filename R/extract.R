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
      j <- 1:nrow(data)
    } else {
      if (is.null(j)) j <- 1:nrow(data)
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
        j <- seq_along(m)
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
#' @aliases getCoordinates,SpaceTime-method
setMethod(
  f = "getCoordinates",
  signature = "SpaceTime",
  definition = function(object) {
    coords <- object@coordinates
    if(!all(lengths(coords) == c(0, 0, 0))) {
      coords <- as.data.frame(coords)
      attr(coords, "epsg") <- object@epsg
      coords
    } else {
      NULL
    }
  }
)

#' @export
#' @rdname access
#' @aliases getDates,SpaceTime-method
setMethod(
  f = "getDates",
  signature = "SpaceTime",
  definition = function(object) {
    dates <- object@dates
    if(!all(lengths(dates) == c(0, 0))) {
      as.data.frame(dates)
    } else {
      NULL
    }
  }
)

#' @export
#' @rdname access
#' @aliases getEPSG,SpaceTime-method
setMethod("getEPSG", "SpaceTime", function(object) object@epsg)

#' @export
#' @rdname access
#' @aliases getID,ANY-method
setMethod("getID", "ANY", function(object) object@id)

#' @export
#' @rdname access
#' @aliases getTotals,FrequencyMatrix-method
setMethod("getTotals", "FrequencyMatrix", function(object) object@totals)

# Setters ======================================================================
#' @export
#' @rdname access
#' @aliases setDates,SpaceTime-method
setMethod(
  f = "setDates<-",
  signature = "SpaceTime",
  definition = function(object, value) {
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
    } else if (is.numeric(value) | is.integer(value)) {
      x <- value
      y <- rep_len(NA_real_, length.out = length(x))
      if (getOption("verbose")) message("errors are missing, NA generated.")
    } else {
      stop("`value` should be a numeric of integer vector, ",
           "a list, a matrix or a data frame.", call. = FALSE)
    }

    object@dates <- list(value = x, error = y)
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname access
#' @aliases setCoordinates,SpaceTime-method
setMethod(
  f = "setCoordinates<-",
  signature = "SpaceTime",
  definition = function(object, value) {
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
      stop("`value` should be a list, a matrix or a data frame.",
           call. = FALSE)
    }
    coords <- list(x = x, y = y, z = z)
    n_coords <- unique(lengths(coords))
    if (length(n_coords) != 1) {
      stop(sprintf("'x', 'y' and 'z' lengths differ (%s).",
                   paste(n_coords, collapse = ", ")), call. = FALSE)
    }
    object@coordinates <- coords
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname access
#' @aliases setEPSG,SpaceTime-method
setMethod(
  f = "setEPSG<-",
  signature = "SpaceTime",
  definition = function(object, value) {
    if (!is.numeric(object) | !is.integer(object) | length(object) != 1)
      stop("`object` should be a length-one numeric or integer vector.",
           call. = FALSE)
    object@epsg <- as.integer(value[1L])
    methods::validObject(object)
    object
  }
)
