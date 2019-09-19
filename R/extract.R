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
    id <- rownames(data)
    if (m != 0) {
      if (missing(j)) {
        j <- seq_len(m)
      } else {
        if (is.null(j)) j <- seq_along(m)
        if (is.character(j) | is.factor(j)) j <- which(id %in% j)
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
#' @rdname geography
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
#' @rdname geography
#' @aliases get_epsg,AbundanceMatrix-method
setMethod("get_epsg", "AbundanceMatrix", function(object) object@epsg)

#' @export
#' @rdname geography
#' @aliases get_features,AbundanceMatrix-method
setMethod(
  f = "get_features",
  signature = "AbundanceMatrix",
  definition = function(object) {
    # Spatial coordinates
    epsg <- get_epsg(object)
    coords <- get_coordinates(object)
    if (any(lengths(coords) == 0)) {
      coords <- matrix(data = rep(NA_real_, 3 * nrow(object)), ncol = 3,
                       dimnames = list(NULL, c("X", "Y", "Z")))
      coords <- as.data.frame(coords)
      message("No coordinates were set, NA generated.")
    }
    # Time coordinates
    dates <- get_dates(object)
    if (any(lengths(dates) == 0)) {
      dates <- matrix(data = rep(NA_real_, 2 * nrow(object)), ncol = 2)
      dates <- as.data.frame(dates)
      message("No dates were set, NA generated.")
    }
    colnames(dates) <- c("DATE_VALUE", "DATE_ERROR")

    # XYZ_index <- !vapply(X = coords, FUN = anyNA, FUN.VALUE = logical(1))
    cbind.data.frame(SITE = rownames(object), coords, dates, object)
  }
)

#' @export
#' @rdname date
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
#' @aliases get_id,ANY-method
setMethod("get_id", "ANY", function(object) object@id)

#' @export
#' @rdname seriation
#' @aliases get_order,PermutationOrder-method
setMethod("get_order", "PermutationOrder", function(object) {
  list(rows = object@rows, columns = object@columns)
})

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
make_dates <- function(value) {
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
    stop("A numeric, integer or character vector, ",
         "a list, a matrix or a data frame is expected.", call. = FALSE)
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
#' @rdname date
#' @aliases set_dates,AbundanceMatrix-method
setMethod(
  f = "set_dates<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    value <- make_dates(value)
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

    if (nrow(value) != 0) rownames(B) <- rows_object
    object@dates <- B
    methods::validObject(object)
    if (getOption("verbose")) {
      message(sprintf(ngettext(k, "%d date was set.", "%d dates were set."), k))
    }
    object
  }
)

# Coordinates ------------------------------------------------------------------
make_coordinates <- function(value) {
  if (is.matrix(value) | is.data.frame(value)) {
    value <- data.matrix(value)
    colnames(value) <- toupper(colnames(value))
    if (ncol(value) >= 2) {
      if (all(c("X", "Y") %in% colnames(value))) {
        X <- value[, "X"]
        Y <- value[, "Y"]
      } else {
        X <- value[, 1]
        Y <- value[, 2]
      }
    } else {
      stop("`value` should have at least 2 columns.", call. = FALSE)
    }
    if (ncol(value) >= 3) {
      if ("Z" %in% colnames(value)) {
        Z <- value[, "Z"]
      } else {
        Z <- value[, 3]
      }
    } else {
      Z <- rep_len(x = NA_real_, length.out = nrow(value))
      if (getOption("verbose")) message("'Z' is missing, NA generated.")
    }
  } else if (is.list(value)) {
    names(value) <- toupper(names(value))
    if (all(c("X", "Y") %in% names(value))) {
      X <- value[["X"]]
      Y <- value[["Y"]]
    } else {
      stop("`value` is a list, but does not have components 'X' and 'Y'.",
           call. = FALSE)
    }
    if ("Z" %in% names(value)) {
      Z <- value[["Z"]]
    } else {
      Z <- rep_len(x = NA_real_, length.out = length(X))
      if (getOption("verbose")) message("'Z' is missing, NA generated.")
    }
  } else {
    stop("A list, a matrix or a data frame is expected.",
         call. = FALSE)
  }
  cbind(X = X, Y = Y, Z = Z)
}

#' @export
#' @rdname geography
#' @aliases set_coordinates,AbundanceMatrix-method
setMethod(
  f = "set_coordinates<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    object@coordinates <- make_coordinates(value)
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname geography
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
