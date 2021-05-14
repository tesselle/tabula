# REFINE MATRIX SERIATION
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriation
#' @aliases refine_seriation,CA-method
setMethod(
  f = "refine_seriation",
  signature = signature(object = "CA"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2),
                        progress = getOption("tabula.progress"), ...) {
    ## Validation
    if (!is.function(cutoff))
      stop(sprintf("%s must be a function.", sQuote("cutoff")), call. = FALSE)

    ## Partial bootstrap CA
    ca_res <- dimensio::bootstrap(object, n = n)
    ca_rep_row <- dimensio::get_replications(ca_res, margin = 1)
    ca_rep_col <- dimensio::get_replications(ca_res, margin = 2)

    hull_rows <- apply(X = ca_rep_row, MARGIN = 1,
                       FUN = function(x, axes) compute_chull(t(x), axes),
                       axes = axes)
    hull_columns <- apply(X = ca_rep_col, MARGIN = 1,
                          FUN = function(x, axes) compute_chull(t(x), axes),
                          axes = axes)

    ## Get convex hull maximal dimension length for each sample
    length_rows <- vapply(
      X = hull_rows,
      FUN = function(x) max(stats::dist(x, method = "euclidean")),
      FUN.VALUE = double(1)
    )
    length_columns <- vapply(
      X = hull_columns,
      FUN = function(x) max(stats::dist(x, method = "euclidean")),
      FUN.VALUE = double(1)
    )

    ## Get cutoff values
    limit_rows <- cutoff(length_rows)
    limit_columns <- cutoff(length_columns)

    ## Samples to be kept
    keep_rows <- which(length_rows < limit_rows)
    keep_columns <- which(length_columns < limit_columns)

    ## Bind hull vertices in a data.frame
    id_rows <- rep(seq_along(hull_rows),
                   times = vapply(hull_rows, nrow, numeric(1)))
    rows <- do.call(rbind, hull_rows)
    rows <- cbind(id = id_rows, rows)

    id_cols <- rep(seq_along(hull_columns),
                   times = vapply(hull_columns, nrow, numeric(1)))
    cols <- do.call(rbind, hull_columns)
    cols <- cbind(id = id_cols, cols)

    .RefineCA(
      object,
      row_chull = rows,
      row_length = length_rows,
      row_keep = keep_rows,
      column_chull = cols,
      column_length = length_columns,
      column_keep = keep_columns,
      cutoff = c(row = limit_rows, column = limit_columns)
    )
  }
)

#' Convex Hull of CA Coordinates
#'
#' Compute convex hull area for each replicated sample
#' @param x A [`numeric`] matrix of bootstrap replicates.
#' @param axes A length-two [`numeric`] vector giving the subscripts
#'  of the CA components to use.
#' @return A [`matrix`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_chull <- function(x, axes) {
  # Remove missing values
  clean <- stats::na.omit(x[, axes])
  # Get convex hull coordinates
  points <- grDevices::chull(clean)
  # Repeat first point
  hull <- clean[c(points, points[1]), axes]
  colnames(hull) <- c("x", "y")

  return(hull)
}
