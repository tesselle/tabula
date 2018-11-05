#' @include AllGenerics.R AllClasses.R index-diversity.R
NULL

# Diversity and dominance index ================================================
#' @export
#' @rdname alpha-diversity
#' @aliases diversity,CountMatrix-method
setMethod(
  f = "diversity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        simplify = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    H <- sapply(X = method, FUN = function(x, data) {
      index <- switch (
        x,
        berger = bergerDominance,
        brillouin = brillouinDiversity,
        # chao = chaoIndex,
        mcintosh = mcintoshDominance,
        shannon = shannonDiversity,
        simpson = simpsonDominance
      )
      apply(X = object, MARGIN = 1, FUN = index)
    }, data = object, simplify = simplify)
    return(H)
  }
)

# Evenness measure =============================================================
#' @export
#' @rdname alpha-diversity
#' @aliases evenness,CountMatrix-method
setMethod(
  f = "evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        simplify = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- sapply(X = method, FUN = function(x, data) {
      index <- switch (
        x,
        # berger = bergerEvenness,
        brillouin = brillouinEvenness,
        # chao = chaoEvenness,
        mcintosh = mcintoshEvenness,
        shannon = shannonEvenness,
        simpson = simpsonEvenness
      )
      apply(X = object, MARGIN = 1, FUN = index)
    }, data = object, simplify = simplify)
    return(E)
  }
)
