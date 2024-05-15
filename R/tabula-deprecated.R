#' Deprecated Functions in tabula
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name tabula-deprecated
#' @keywords internal
NULL

#' @rdname tabula-deprecated
#' @aliases test_diversity-method
setGeneric(
  name = "test_diversity",
  def = function(object, ...) standardGeneric("test_diversity")
)

#' @export
#' @rdname tabula-deprecated
#' @aliases test_diversity,matrix-method
setMethod(
  f = "test_diversity",
  signature = signature(object = "matrix"),
  definition = function(object, adjust = "holm", ...) {
    .Deprecated(new = "test_shannon()", old = "test_diversity()")

    # Calculate the number of individuals
    N <- rowSums(object)
    # Calculate Shannon diversity
    H <- apply(X = object, MARGIN = 1, FUN = index_shannon, ...)
    # Calculate Shannon variance
    V <- apply(X = object, MARGIN = 1, FUN = variance_shannon, ...)
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

#' @export
#' @rdname tabula-deprecated
#' @aliases test_diversity,data.frame-method
setMethod(
  f = "test_diversity",
  signature = signature(object = "data.frame"),
  definition = function(object, adjust = "holm", ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, adjust = adjust)
  }
)
