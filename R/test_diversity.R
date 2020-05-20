#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname test_diversity
#' @aliases test_diversity,CountMatrix-method
setMethod(
  f = "test_diversity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, adjust = "holm", ...) {
    # Calculate the number of individuals
    N <- apply(X = object, MARGIN = 1, FUN = sum)
    # Calculate Shannon diversity
    H <- apply(X = object, MARGIN = 1, FUN = diversityShannon, ...)
    # Calculate Shannon variance
    V <- apply(X = object, MARGIN = 1, FUN = varianceShannon, ...)
    # Get the names of the assemblages
    row_names <- rownames(object)
    if (length(row_names) != 0) {
      row_names <- factor(row_names, levels = unique(row_names))
    } else {
      row_names <- factor(seq_len(nrow(object)))
    }
    # Compute t test
    compare <- function(i, j) {
      tt <- (H[i] - H[j]) / sqrt(V[i] + V[j])
      df <- (V[i] + V[j])^2 / sum(V[c(i, j)]^2 / N[c(i, j)])
      2 * (1 - stats::pt(q = abs(tt), df = df))
    }
    result <- stats::pairwise.table(
      compare.levels = compare,
      level.names = row_names,
      p.adjust.method = adjust
    )
    return(result)
  }
)
