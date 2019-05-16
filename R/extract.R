# ACCESSORS
#' @include AllClasses.R
NULL

# BootCA =======================================================================
#' @export
#' @rdname extract-method
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
    return(data)
  }
)
#' @export
#' @rdname extract-method
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
    return(data)
  }
)

# ------------------------------------------------------------------------------
#' Helper to Extract Parts of an Object
#'
#' @inheritParams extract
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
extractSlot <- function(x, i) {
  class_name <- class(x)
  i <- match.arg(i, choices = methods::slotNames(class_name),
                 several.ok = FALSE)
  data <- methods::slot(x, i)
  return(data)
}

#' @export
#' @rdname extract-method
#' @aliases [[,DateModel-method
setMethod(
  f = "[[",
  signature = "DateModel",
  definition = extractSlot
)
#' @export
#' @rdname extract-method
#' @aliases [[,BootCA-method
setMethod(
  f = "[[",
  signature = "BootCA",
  definition = function(x, i) {
    data <- extractSlot(x, i)

    if(i %in% c("rows", "columns")) {
      data <- as.data.frame(data)
    }
    return(data)
  }
)
#' @export
#' @rdname extract-method
#' @aliases [[,PermutationOrder-method
setMethod(
  f = "[[",
  signature = "PermutationOrder",
  definition = extractSlot
)
#' @export
#' @rdname extract-method
#' @aliases [[,CountMatrix-method
setMethod(
  f = "[[",
  signature = "CountMatrix",
  definition = extractSlot
)
#' @export
#' @rdname extract-method
#' @aliases [[,FrequencyMatrix-method
setMethod(
  f = "[[",
  signature = "FrequencyMatrix",
  definition = extractSlot
)
#' @export
#' @rdname extract-method
#' @aliases [[,IncidenceMatrix-method
setMethod(
  f = "[[",
  signature = "IncidenceMatrix",
  definition = extractSlot
)
#' @export
#' @rdname extract-method
#' @aliases [[,OccurrenceMatrix-method
setMethod(
  f = "[[",
  signature = "OccurrenceMatrix",
  definition = extractSlot
)
#' @export
#' @rdname extract-method
#' @aliases [[,SimilarityMatrix-method
setMethod(
  f = "[[",
  signature = "SimilarityMatrix",
  definition = extractSlot
)

# Access =======================================================================
#' @export
#' @rdname access-method
#' @aliases getID,ANY-method
setMethod("getID", "ANY", function(x) x@id)

#' @export
#' @rdname access-method
#' @aliases getTotals,FrequencyMatrix-method
setMethod("getTotals", "FrequencyMatrix", function(x) x@totals)
