#' @include AllGenerics.R AllClasses.R index-richness.R
NULL

# Richness =====================================================================
#' @export
#' @rdname richness-method
#' @aliases richness,CountMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("margalef", "menhinick"),
                        simplify = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- sapply(X = method, FUN = function(x, object) {
      index <- switch (
        x,
        margalef = margalefRichness,
        menhinick = menhinickRichness
      )
      apply(X = object, MARGIN = 1, FUN = index, x)
    }, object, simplify = simplify)
    return(E)
  }
)

#' @export
#' @rdname richness-method
#' @aliases richness,IncidenceMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object) {
    E <- apply(X = object, MARGIN = 1, FUN = function(x) sum(x > 0))
    return(E)
  }
)

# Rarefaction ==================================================================
#' @export
#' @rdname richness-method
#' @aliases rarefaction,CountMatrix-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "CountMatrix"),
  definition = function(object, sample) {
    E <- apply(X = object, MARGIN = 1, FUN = hurlbertRarefaction, sample)
    return(E)
  }
)

