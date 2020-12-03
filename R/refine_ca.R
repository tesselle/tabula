# REFINE MATRIX SERIATION
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname refine
#' @aliases refine_ca,CA-method
setMethod(
  f = "refine_ca",
  signature = signature(object = "CA"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2),
                        progress = getOption("tabula.progress"), ...) {
    data <- object

    # Partial bootstrap CA
    hull_rows <- boot_ca(data, fun = compute_ca_chull, margin = 1, n = n,
                         progress = progress, axes = axes)
    hull_columns <- boot_ca(data, fun = compute_ca_chull, margin = 2, n = n,
                            progress = progress, axes = axes, ...)

    # Get convex hull maximal dimension length for each sample
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

    # Get cutoff values
    limit_rows <- cutoff(length_rows)
    limit_columns <- cutoff(length_columns)

    # Samples to be kept
    keep_rows <- which(length_rows < limit_rows)
    keep_columns <- which(length_columns < limit_columns)

    # Bind hull vertices in a data.frame
    id_rows <- rep(names(hull_rows),
                   times = vapply(hull_rows, nrow, numeric(1)))
    rows <- do.call(rbind.data.frame, hull_rows)
    rows <- data.frame(id = id_rows, rows, stringsAsFactors = FALSE)

    id_cols <- rep(names(hull_columns),
                   times = vapply(hull_columns, nrow, numeric(1)))
    cols <- do.call(rbind.data.frame, hull_columns)
    cols <- data.frame(id = id_cols, cols, stringsAsFactors = FALSE)

    results <- methods::as(data, "BootCA")
    results@row_chull <- rows
    results@column_chull <- cols
    results@lengths <- list(length_rows, length_columns)
    results@cutoff <- c(limit_rows, limit_columns)
    results@keep <- list(keep_rows, keep_columns)
    results
  }
)

#' Apply Function Over Bootstrap Replicates
#'
#' @param x A \linkS4class{CA} object.
#' @param fun A \code{\link{function}}.
#' @param margin A length-one \code{\link{numeric}} vector giving the subscripts
#'  which the function will be applied over: \code{1} indicates rows, \code{2}
#'  indicates columns.
#' @param n A non-negative \code{\link{integer}} giving the number of bootstrap
#'  replications.
#' @param progress A \code{\link{logical}} scalar: should a progress bar be
#'  displayed?
#' @param ... Further parameters to be passed to \code{fun}.
#' @return A \code{\link{list}}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
boot_ca <- function(x, fun, margin = 1, n = 1000,
                    progress = getOption("tabula.progress"), ...) {

  arg <- deparse(substitute(fun))
  if (!is.function(fun))
    stop(sprintf("%s must be a function.", sQuote(arg)))

  ## Get CA standard coordinates (SVD)
  svd <- if (margin == 1) x[["column_svd"]] else x[["row_svd"]]
  data <- if (margin == 1)  x[["data"]] else t(x[["data"]])

  m <- nrow(data)
  k <- seq_len(m)
  boot <- vector(mode = "list", length = m)

  progress_bar <- interactive() && progress
  if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

  for (i in k) {
    # n random replicates
    sample <- data[i, , drop = TRUE]
    replicates <- stats::rmultinom(n = n, size = sum(sample), prob = sample)

    # Compute new CA coordinates
    coords <- crossprod(replicates / colSums(replicates), svd)

    # Apply on new CA coordinates
    boot[[i]] <- fun(x = coords, ...)
    if (progress_bar) utils::setTxtProgressBar(pbar, i)
  }

  if (progress_bar) close(pbar)

  names(boot) <- rownames(data)
  boot
}

#' Convex Hull of CA Coordinates
#'
#' Compute convex hull area for each replicated sample
#' @param x A \code{\link{numeric}} matrix of bootstrap replicates.
#' @param axes A length-two \code{\link{numeric}} vector giving the subscripts
#'  of the CA components to use.
#' @return A \code{\link{data.frame}}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_ca_chull <- function(x, axes) {
  # Get convex hull coordinates
  points <- grDevices::chull(x[, axes])
  # Repeat first point
  hull <- x[c(points, points[1]), axes]
  colnames(hull) <- c("x", "y")

  return(as.data.frame(hull))
}
