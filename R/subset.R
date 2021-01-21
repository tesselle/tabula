# ACCESSORS
#' @include AllClasses.R
NULL

# ====================================================================== Extract
#' @export
#' @rdname subset
#' @aliases [,BootCA-method
setMethod(
  f = "[",
  signature = "BootCA",
  definition = function(x, i, j, drop = TRUE) {
    i <- match.arg(i, choices = c("row_chull", "column_chull"),
                   several.ok = FALSE)
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
    i <- match.arg(i, choices = c("data"),
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

#' @export
#' @rdname subset
#' @aliases [,DateEvent-method
setMethod(
  f = "[",
  signature = "DateEvent",
  definition = function(x, i, j, drop = TRUE) {
    i <- match.arg(i, choices = c("data", "row_events", "column_events",
                                  "accumulation"), several.ok = FALSE)
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

#' Extract Parts of an Object
#'
#' @inheritParams subset
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
extract_slot <- function(x, i) {
  class_name <- class(x)
  i <- match.arg(i, choices = methods::slotNames(class_name),
                 several.ok = FALSE)
  data <- methods::slot(x, i)
  data
}

#' @export
#' @rdname subset
#' @aliases [[,DiversityIndex-method
setMethod(
  f = "[[",
  signature = "DiversityIndex",
  definition = extract_slot
)

#' @export
#' @rdname subset
#' @aliases [[,IncrementTest-method
setMethod(
  f = "[[",
  signature = "IncrementTest",
  definition = extract_slot
)

#' @export
#' @rdname subset
#' @aliases [[,DateModel-method
setMethod(
  f = "[[",
  signature = "DateModel",
  definition = extract_slot
)

#' @export
#' @rdname subset
#' @aliases [[,DateEvent-method
setMethod(
  f = "[[",
  signature = "DateEvent",
  definition = extract_slot
)

#' @export
#' @rdname subset
#' @aliases [[,BootCA-method
setMethod(
  f = "[[",
  signature = "BootCA",
  definition = function(x, i) {
    data <- extract_slot(x, i)
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
  definition = extract_slot
)
