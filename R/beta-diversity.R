#' @include AllGenerics.R AllClasses.R index-similarity.R index-turnover.R
NULL

# Turnover measure =============================================================
turnoverIndex <- function(object, method, simplify = FALSE, ...) {
  # Validation -----------------------------------------------------------------
  index <- switch (
    method,
    whittaker = whittakerBeta,
    cody = codyBeta,
    routledge1 = routledge1Beta,
    routledge2 = routledge2Beta,
    routledge3 = routledge3Beta,
    wilson = wilsonBeta,
    stop(paste("there is no such method:", method, sep = " "))
  )
  B <- index(object)
  return(B)
}

#' @export
#' @rdname beta-diversity
#' @aliases turnover,CountMatrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), simplify = FALSE, ...) {
    method <- match.arg(method, several.ok = TRUE)
    B <- sapply(X = method, FUN = function(x, data) {
      turnoverIndex(data, x)
    }, data = object, simplify = simplify)
    return(B)
  }
)

#' @export
#' @rdname beta-diversity
#' @aliases turnover,FrequencyMatrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), simplify = FALSE, ...) {
    method <- match.arg(method, several.ok = TRUE)
    B <- turnoverIndex(object, method)
    return(B)
  }
)

#' @export
#' @rdname beta-diversity
#' @aliases turnover,IncidenceMatrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), simplify = FALSE, ...) {
    method <- match.arg(method, several.ok = TRUE)
    B <- turnoverIndex(object, method)
    return(B)
  }
)

# Similarity measure ===========================================================
similarityIndex <- function(object, method, ...) {
  index <- switch (
    method,
    bray = braySimilarity,
    jaccard = jaccardSimilarity,
    morisita = morisitaSimilarity,
    sorenson = sorensonSimilarity,
    stop(paste("there is no such method:", method, sep = " "))
  )
  # Pairwise comparison
  m <- nrow(object)
  beta <- apply(X = combn(1:m, 2), MARGIN = 2, FUN = function(x) {
    index(object[x[1], ], object[x[2], ])
  })
  # Matrix of results
  m <- nrow(object)
  labels <- rownames(object)
  C <- matrix(data = 1, nrow = m, ncol = m, dimnames = list(labels, labels))
  C[col(C) > row(C)] <- beta
  C[row(C) > col(C)] <- beta

  return(C)
}

#' @export
#' @rdname beta-diversity
#' @aliases similarity,CountMatrix-method
setMethod(
  f = "similarity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("bray", "jaccard", "morisita",
                                           "sorenson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    C <- similarityIndex(object, method)
    return(C)
  }
)

#' @export
#' @rdname beta-diversity
#' @aliases similarity,FrequencyMatrix-method
setMethod(
  f = "similarity",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, method = c("bray", "jaccard", "morisita",
                                           "sorenson"), ...) {
    object <- as(object, "CountMatrix")
    C <- similarity(object, method)
    return(C)
  }
)

#' @export
#' @rdname beta-diversity
#' @aliases similarity,CountMatrix-method
setMethod(
  f = "similarity",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("jaccard", "sorenson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    C <- similarityIndex(object, method)
    return(C)
  }
)
