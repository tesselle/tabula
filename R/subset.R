# ACCESSORS
#' @include AllClasses.R
NULL

# Extract ======================================================================
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
#' @aliases [[,DiversityIndex,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "DiversityIndex", i = "ANY", j = "missing"),
  definition = extract_slot
)

#' @export
#' @rdname subset
#' @aliases [[,RefineCA,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "RefineCA", i = "ANY", j = "missing"),
  definition = function(x, i) {
    i <- match.arg(i, choices = c("rows", "columns", "cutoff"),
                   several.ok = FALSE)
    switch (
      i,
      rows = list(
        "chull" = x@row_chull,
        "length" = x@row_length,
        "keep" = x@row_keep
      ),
      columns = list(
        "chull" = x@column_chull,
        "length" = x@column_length,
        "keep" = x@column_keep
      ),
      cutoff = x@cutoff
    )
  }
)

#' @export
#' @rdname subset
#' @aliases [[,PermutationOrder,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PermutationOrder", i = "ANY", j = "missing"),
  definition = extract_slot
)
