#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname refine
#' @aliases refine_diversity,HeterogeneityIndex-method
setMethod(
  f = "refine_diversity",
  signature = signature(object = "HeterogeneityIndex"),
  definition = function(object, method = c("jackknife", "bootstrap"),
                        probs = c(0.05, 0.95), n = 1000, ...) {
    # Select index
    index <- switch_heterogeneity(object[["method"]])

    refine_index(object = object, index = index, method = method,
                 probs = probs, n = n)
  }
)

#' @export
#' @rdname refine
#' @aliases refine_diversity,EvennessIndex-method
setMethod(
  f = "refine_diversity",
  signature = signature(object = "EvennessIndex"),
  definition = function(object, method = c("jackknife", "bootstrap"),
                        probs = c(0.05, 0.95), n = 1000, ...) {
    # Select method
    index <- switch_evenness(object[["method"]])

    refine_index(object = object, index = index, method = method,
                 probs = probs, n = n)
  }
)

# @export
# @rdname refine
# @aliases refine_diversity,RichnessIndex-method
# setMethod(
#   f = "refine_diversity",
#   signature = signature(object = "RichnessIndex"),
#   definition = function(object, method = c("jackknife", "bootstrap"),
#                         probs = c(0.05, 0.95), n = 1000, ...) {
#     # Select method
#     index <- switch_richness(object[["method"]])
#
#     refine_index(object = object, index = index, method = method,
#                  probs = probs, n = n)
#   }
# )

refine_index <- function(object, index, method = c("jackknife", "bootstrap"),
                         probs = c(0.05, 0.95), n = 1000) {
  # Validation
  method <- match.arg(method, several.ok = FALSE)
  x <- object[["data"]]

  # Jackknife
  if ("jackknife" %in% method) {
    results <- apply(
      X = x,
      MARGIN = 1,
      FUN = function(x, do) {
        jackknife(x, do)[c("mean", "bias", "error")]
      },
      do = index
    )
  }
  # Bootstrap
  if ("bootstrap" %in% method) {
    results <- apply(X = x, MARGIN = 1, FUN = bootstrap,
                     do = index, probs = probs, n = n)
  }
  results <- do.call(rbind, results)
  results
}
