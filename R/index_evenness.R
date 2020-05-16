#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname heterogeneity-index
#' @aliases index_evenness,CountMatrix-method
setMethod(
  f = "index_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("shannon", "brillouin", "mcintosh",
                                           "simpson"),
                        simulate = FALSE, quantiles = TRUE, level = 0.80,
                        step = 1, n = 1000, ...) {
    # Select method
    fun <- switch_evenness(method)

    index <- index_diversity(object, fun, simulate = simulate, prob = NULL,
                             quantiles = quantiles, level = level,
                             step = step, n = n, ...)

    index <- methods::as(index, "EvennessIndex")
    index@method <- method[[1L]]
    index
  }
)

switch_evenness <- function(x = c("shannon", "brillouin", "mcintosh",
                                  "simpson")) {
  # Validation
  x <- match.arg(x, several.ok = FALSE)

  index <- switch (
    x,
    brillouin = evennessBrillouin,
    mcintosh = evennessMcintosh,
    shannon = evennessShannon,
    simpson = evennessSimpson,
    stop(sprintf("There is no such method: %s.", x), call. = FALSE)
  )
  return(index)
}
