# ACCESSORS
#' @include AllClasses.R
NULL

# BootCA =======================================================================
#' @export
#' @describeIn BootCA Extracts informations from a slot selected by subscript
#'  \code{i}.
#' @aliases [[,BootCA-method
setMethod(
  f = "[[",
  signature = "BootCA",
  definition = function(x, i) {
    i <- match.arg(i, choices = methods::slotNames("BootCA"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)

#' @export
#' @describeIn BootCA Allows to select a slot thru \code{j}.
#' @aliases [,BootCA-method
setMethod(
  f = "[",
  signature = "BootCA",
  definition = function(x, i, j) {
    i <- match.arg(i, choices = c("rows", "columns", "lengths"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)

    if (missing(j)) {
      j <- 1:nrow(data)
    } else {
      if (is.null(j)) j <- 1:nrow(data)
      if (is.character(j) | is.factor(j)) j <- which(data$id %in% j)
      if (is.numeric(j)) j <- as.integer(j)
    }
    data <- data[j, ]
    return(data)
  }
)

# DateModel ====================================================================
#' @export
#' @describeIn DateModel Extracts informations from a slot selected by subscript
#'  \code{i}.
#' @aliases [[,DateModel-method
setMethod(
  f = "[[",
  signature = "DateModel",
  definition = function(x, i){
    i <- match.arg(i, choices = methods::slotNames("DateModel"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)

#' @export
#' @describeIn DateModel Allows to select a slot thru \code{j}.
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

# PermutationOrder =============================================================
#' @export
#' @param x A \code{PermutationOrder} object from which to extract element(s).
#' @param i A \code{\link{character}} string specifying the element to extract.
#'  Character vectors will be matched to the name of the slots.
#' @describeIn PermutationOrder Extracts informations from a slot selected by
#'  subscript \code{i}.
#' @aliases [[,PermutationOrder-method
setMethod(
  f = "[[",
  signature = "PermutationOrder",
  definition = function(x, i){
    i <- match.arg(i, choices = methods::slotNames("PermutationOrder"),
                   several.ok = FALSE)
    data <- methods::slot(x, i)
    return(data)
  }
)

# ==============================================================================
#' Accessors
#'
#' @param x An object from which to extract element(s).
#' @author N. Frerebeau
#' @docType methods
#' @name accessors
#' @rdname accessors
NULL

#' @aliases totals
#' @rdname accessors
#' @export
setGeneric("totals", function(x) standardGeneric("totals"))

#' @export
#' @describeIn FrequencyMatrix Returns the row sums (counts).
#' @aliases totals,FrequencyMatrix-method
setMethod("totals", "FrequencyMatrix", function(x) x@totals)
