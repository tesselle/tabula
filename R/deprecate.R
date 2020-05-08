# DEPRECATED METHODS
NULL

#' @export
#' @rdname deprecate
#' @aliases seriate_reciprocal,CountMatrix-method
setMethod(
  f = "seriate_reciprocal",
  signature = signature(object = "CountMatrix"),
  definition = function(object, EPPM = FALSE, margin = c(1, 2), stop = 100) {
    .Deprecated(new = "seriate_rank")
    seriate_rank(object, margin = margin, stop = stop, EPPM = EPPM)
  }
)

#' @export
#' @rdname deprecate
#' @aliases seriate_reciprocal,IncidenceMatrix-method
setMethod(
  f = "seriate_reciprocal",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, margin = c(1, 2), stop = 100) {
    .Deprecated(new = "seriate_rank")
    seriate_rank(object, margin = margin, stop = stop)
  }
)

#' @export
#' @rdname deprecate
#' @aliases refine_seriation,CountMatrix-method
setMethod(
  f = "refine_seriation",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2), ...) {
    .Deprecated(new = "refine_seriation")
    object <- run_ca(object)
    refine_ca(object, cutoff, n, axes, ...)
  }
)

#' @export
#' @rdname plot_date
#' @aliases plot_time,CountMatrix,missing-method
setMethod(
  f = "plot_time",
  signature = signature(object = "CountMatrix", dates = "missing"),
  definition = function(object, highlight = NULL, level = 0.95,
                        roll = FALSE, window = 5, facet = TRUE, ...) {
    warning("This method is deprecated: please provide dates explicitly.",
            call. = FALSE)
    dates <- get_dates(object)[["value"]]
    if (length(dates) == 0) stop("Time coordinates are missing!", call. = FALSE)
    plot_time(object, dates, highlight = highlight, level = level,
              roll = roll, window = window, facet = facet, ...)
  }
)
