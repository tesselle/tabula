#' @include AllGenerics.R AllClasses.R index-diversity.R index-richness.R
NULL

# Richness =====================================================================
#' @export
#' @rdname alpha-diversity
#' @aliases richness,CountMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("ace", "chao1", "chao1bc", "chao1i",
                                           "margalef", "menhinick"),
                        k = 10, simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- sapply(X = method, FUN = function(x, object, k) {
      index <- switch (
        x,
        ace = aceRichness,
        chao1 = chao1Richness,
        chao1bc = chao1bcRichness,
        chao1i = chao1iRichness,
        margalef = margalefRichness,
        menhinick = menhinickRichness
      )
      apply(X = object, MARGIN = 1, FUN = index, k)
    }, object, k, simplify = simplify)
    return(E)
  }
)

#' @export
#' @rdname alpha-diversity
#' @aliases richness,IncidenceMatrix-method
setMethod(
  f = "richness",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("chao2", "chao2i", "ice"), k = 10,
                        simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)
    index <- switch (
      method,
      ice = iceRichness,
      chao2 = chao2Richness,
      chao2i = chao2iRichness
    )
    E <- index(object, k)
    return(E)
  }
)

# Rarefaction ==================================================================
#' @export
#' @rdname alpha-diversity
#' @aliases rarefaction,CountMatrix-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "CountMatrix"),
  definition = function(object, sample) {
    E <- apply(X = object, MARGIN = 1, FUN = hurlbertRarefaction, sample)
    return(E)
  }
)

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
  definition = function(object, method = c("brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        simplify = FALSE, ...) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- sapply(X = method, FUN = function(x, data) {
      index <- switch (
        x,
        brillouin = brillouinEvenness,
        mcintosh = mcintoshEvenness,
        shannon = shannonEvenness,
        simpson = simpsonEvenness
      )
      apply(X = object, MARGIN = 1, FUN = index)
    }, data = object, simplify = simplify)
    return(E)
  }
)
