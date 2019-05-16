# REFINE MATRIX SERIATION
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname seriation
#' @aliases refine,CountMatrix-method
setMethod(
  f = "refine",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2), ...) {
    # Partial bootstrap CA
    hull_rows <- bootHull(object, n = n, margin = 1, axes = axes, ...)
    hull_columns <- bootHull(object, n = n, margin = 2, axes = axes, ...)
    # Get convex hull maximal dimension length for each sample
    length_rows <- sapply(X = hull_rows, function(x) {
      max(stats::dist(x, method = "euclidean"))
    })
    length_columns <- sapply(X = hull_columns, function(x) {
      max(stats::dist(x, method = "euclidean"))
    })
    # Get cutoff values
    limit_rows <- cutoff(length_rows)
    limit_columns <- cutoff(length_columns)
    # Samples to be kept
    keep_rows <- which(length_rows < limit_rows)
    keep_columns <- which(length_columns < limit_columns)
    # Bind hull vertices in a data.frame
    rows <- dplyr::bind_rows(hull_rows, .id = "id")
    cols <- dplyr::bind_rows(hull_columns, .id = "id")

    BootCA(
      id = object[["id"]],
      rows = as.list(rows), columns = as.list(cols),
      lengths = list(length_rows, length_columns),
      cutoff = c(limit_rows, limit_columns),
      keep = list(keep_rows, keep_columns)
    )
  }
)

#' @param x An \eqn{m \times p}{m x p} data matrix.
#' @param cutoff A function that takes a numeric vector as argument and returns
#'  a single numeric value.
#' @param margin A non-negative \code{\link{integer}} giving the subscripts
#'  which the rearrangement will be applied over: \code{1} indicates rows,
#'  \code{2} indicates columns.
#' @param n A non-negative \code{\link{integer}} giving the number of partial
#'  bootstrap replications.
#' @param axes A \code{\link{numeric}} vector giving the subscripts of the CA
#'  components to use.
#' @return A list of \code{\link[=data.frame]{data frames}} giving the convex
#'  hull coordinates for each replicated sample.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
bootHull <- function(x, margin = 1, n = 1000, axes = c(1, 2), ...) {
  # Validation
  margin <- as.integer(margin)
  n <- as.integer(n)
  axes <- as.integer(axes)

  # CA on the whole dataset
  results_CA <- FactoMineR::CA(x, graph = FALSE, ...)
  svd <- if (margin == 1) results_CA$svd$V else results_CA$svd$U

  # Compute convex hull area for each replicated sample
  computeHull <- function(x, n, svd, axes) {
    # n random replicates
    replicated <- stats::rmultinom(n = n, size = sum(x), prob = x)
    # Compute new CA coordinates
    coords <- crossprod(replicated / colSums(replicated), svd)

    # Get convex hull coordinates
    points <- grDevices::chull(coords[, axes])
    # Repeat first point
    hull <- coords[c(points, points[1]), axes]
    colnames(hull) <- c("x", "y")

    return(as.data.frame(hull))
  }

  hull <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbapply(X = x, MARGIN = margin,
                     FUN = computeHull, n = n, svd = svd, axes = axes)
  } else {
    apply(X = x, MARGIN = margin,
          FUN = computeHull, n = n, svd = svd, axes = axes)
  }

  return(hull)
}
