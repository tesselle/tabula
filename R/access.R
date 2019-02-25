# ACCESSORS
#' @include AllClasses.R
NULL

# BootCA =======================================================================
#' @export
#' @param x A \code{BootCA} object from which to extract element(s).
#' @param i A \code{\link{character}} string specifying elements to extract.
#'  Character vectors will be matched to the name of the slots.
#' @describeIn BootCA Returns information about the individual slots.
#' @aliases [[,BootCA-method
setMethod(
  f = "[[",
  signature = "BootCA",
  definition = function(x, i){
    i <- match.arg(i, choices = methods::slotNames("BootCA"),
                   several.ok = FALSE)
    slot <- slot(x, i)
    return(slot)
  }
)

# DateModel ====================================================================
#' @export
#' @param x A \code{DateModel} object from which to extract element(s).
#' @param i A \code{\link{character}} string specifying elements to extract.
#'  Character string will be matched to the name of the slots.
#' @describeIn DateModel Returns information about the individual slots.
#' @aliases [[,DateModel-method
setMethod(
  f = "[[",
  signature = "DateModel",
  definition = function(x, i){
    i <- match.arg(i, choices = methods::slotNames("DateModel"),
                   several.ok = FALSE)
    slot <- slot(x, i)
    return(slot)
  }
)

# PermutationOrder =============================================================
#' @export
#' @param x A \code{PermutationOrder} object from which to extract element(s).
#' @param i A \code{\link{character}} string specifying elements to extract.
#'  Character vectors will be matched to the name of the slots.
#' @describeIn PermutationOrder Returns information about the individual slots.
#' @aliases [[,PermutationOrder-method
setMethod(
  f = "[[",
  signature = "PermutationOrder",
  definition = function(x, i){
    i <- match.arg(i, choices = methods::slotNames("PermutationOrder"),
                   several.ok = FALSE)
    slot <- slot(x, i)
    return(slot)
  }
)

#' Accessors
#'
#' @param x An object.
#' @author N. Frerebeau
#' @docType methods
#' @name accessors
#' @rdname accessors
NULL

#' @rdname accessors
setGeneric("accumulation", function(x) standardGeneric("accumulation"))

#' @rdname accessors
setGeneric("columns", function(x) standardGeneric("columns"))

#' @rdname accessors
setGeneric("rows", function(x) standardGeneric("rows"))

#' @rdname accessors
setGeneric("totals", function(x) standardGeneric("totals"))

#' @export
#' @describeIn PermutationOrder Returns the rows permutation.
#' @aliases rows,PermutationOrder-method
setMethod("rows", "PermutationOrder", function(x) x@rows)

#' @export
#' @describeIn BootCA Returns the convex hull vertice coordinates for each
#'  individual.
#' @aliases rows,BootCA-method
setMethod("rows", "BootCA", function(x) x@rows)

#' @export
#' @describeIn DateModel Returns a numeric matrix giving the predicted event
#'  dates for each archaeological assemblage, the corresponding confidence
#'  interval and standard error of the predicted dates.
#' @aliases rows,DateModel-method
setMethod("rows", "DateModel", function(x) x@rows)

#' @export
#' @describeIn PermutationOrder Returns the columns permutation.
#' @aliases columns,PermutationOrder-method
setMethod("columns", "PermutationOrder", function(x) x@columns)

#' @export
#' @describeIn BootCA Returns the convex hull vertice coordinates for each
#'  variable.
#' @aliases columns,BootCA-method
setMethod("columns", "BootCA", function(x) x@columns)

#' @export
#' @describeIn DateModel Returns a numeric matrix giving the predicted event
#'  dates for each archaeological type or fabric, the corresponding confidence
#'  interval and standard error of the predicted dates.
#' @aliases rows,DateModel-method
setMethod("columns", "DateModel", function(x) x@columns)

#' @export
#' @describeIn DateModel Returns the accumulation date for each archaeological
#'  assemblage.
#' @aliases method,DateModel-method
setMethod("accumulation", "DateModel", function(x) x@accumulation)

#' @export
#' @describeIn FrequencyMatrix Returns the row sums (counts).
#' @aliases totals,FrequencyMatrix-method
setMethod("totals", "FrequencyMatrix", function(x) x@totals)
