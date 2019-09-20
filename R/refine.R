# REFINE MODELS (MATRIX SERIATION AND DATING)
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname seriation
#' @aliases refine_seriation,CountMatrix-method
setMethod(
  f = "refine_seriation",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2), ...) {
    # Partial bootstrap CA
    hull_rows <- boot_hull(object, n = n, margin = 1, axes = axes, ...)
    hull_columns <- boot_hull(object, n = n, margin = 2, axes = axes, ...)
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
    rows <- cbind.data.frame(id = id_rows, rows)
    id_cols <- rep(names(hull_columns),
                   times = vapply(hull_columns, nrow, numeric(1)))
    cols <- do.call(rbind.data.frame, hull_columns)
    cols <- cbind.data.frame(id = id_cols, cols)

    BootCA(
      id = object[["id"]],
      rows = as.list(rows),
      columns = as.list(cols),
      lengths = list(length_rows, length_columns),
      cutoff = c(limit_rows, limit_columns),
      keep = list(keep_rows, keep_columns)
    )
  }
)

#' Convex Hull of CA Coordinates
#'
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
boot_hull <- function(x, margin = 1, n = 1000, axes = c(1, 2), ...) {
  # Validation
  margin <- as.integer(margin)
  n <- as.integer(n)
  axes <- as.integer(axes)

  # CA on the whole dataset
  ## Compute correspondence analysis
  results_CA <- ca::ca(x, ...)
  ## Get standard coordinates
  std <- ca::cacoord(results_CA, type = "standard")
  svd <- if (margin == 1) std$columns else std$rows

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

  loop_fun <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbapply
  } else {
    apply
  }
  loop_args <- list(X = x, MARGIN = margin, FUN = computeHull,
                    n = n, svd = svd, axes = axes)
  hull <- do.call(loop_fun, loop_args)

  hull
}

# DateModel ====================================================================
#' @export
#' @rdname date
#' @aliases refine_dates,DateModel-method
setMethod(
  f = "refine_dates",
  signature = signature(object = "DateModel"),
  definition = function(object, method = c("jackknife", "bootstrap"),
                        n = 1000, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    # Get data
    counts <- object@counts
    fit <- object@model
    keep_dim <- seq_len(ncol(fit$model) - 1)
    level <- object@level
    row_event <- object@rows

    # Check model with resampling methods
    ## Jackknife fabrics
    if ("jackknife" %in% method) {
      jack_event <- jack_date(counts, fit, keep = keep_dim, level = level, ...)
      # Compute jaccknife bias
      results <- rownames_to_column(jack_event, factor = TRUE, id = "id")
      results <- cbind.data.frame(
        id = results$id,
        date = results$date,
        lower = results$lower,
        upper = results$upper,
        error = results$error,
        bias = (ncol(counts) - 1) * (results$date - row_event[, "date"])
      )
    }
    ## Bootstrap assemblages
    if ("bootstrap" %in% method) {
      results <- boot_date(counts, fit, margin = 1, n = n,
                          keep = keep_dim, level = level, ...)
    }
    results
  }
)

#' Bootstrap Resampling of Assemblages
#'
#' @param x An \eqn{m \times p}{m x p} \code{\link{numeric}} matrix of count data.
#' @param model An object of class \code{\link[stats]{lm}}.
#' @param margin A non-negative \code{\link{integer}} giving the subscripts
#'  which the rearrangement will be applied over: \code{1} indicates rows,
#'  \code{2} indicates columns.
#' @param n A non-negative \code{\link{integer}} giving the number of partial
#'  bootstrap replications.
#' @param keep An \code{\link{integer}} giving the number of CA factors to keep
#'  for new data prediction.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param ... Further arguments to be passed to \code{\link[ca]{ca}}.
#' @return A six columns \code{\link{data.frame}} giving the boostrap
#'  distribution statistics for each replicated assemblage (in rows)
#'  with the following columns:
#'  \describe{
#'   \item{id}{An identifier to link each row to an assemblage.}
#'   \item{min}{Minimum value.}
#'   \item{Q05}{Sample quantile to 0.05 probability.}
#'   \item{mean}{Mean value (event date).}
#'   \item{Q95}{Sample quantile to 0.95 probability.}
#'   \item{max}{Maximum value.}
#'  }
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
boot_date <- function(x, model, margin = 1, n = 1000, keep = ncol(x),
                     level = 0.95, ...) {
  # Validation
  margin <- as.integer(margin)
  n <- as.integer(n)
  keep <- as.integer(keep)
  level <- as.numeric(level)

  # CA on the whole dataset
  axes <- min(dim(x))
  ## Compute correspondence analysis
  results_CA <- ca::ca(x, nd = axes, ...)
  ## Get standard coordinates
  std <- ca::cacoord(results_CA, type = "standard")
  svd <- if (margin == 1) std$columns else std$rows

  # Compute date event statistics for each replicated sample
  compute_stats <- function(x, n, svd, keep, model, level) {
    # n random replicates
    # replicated <- sample(x = n, size = sum(x), prob = x)
    replicated <- stats::rmultinom(n = n, size = sum(x), prob = x)
    # Compute new CA coordinates
    coords <- crossprod(replicated / colSums(replicated), svd)
    coords <- coords[, keep]
    # Gaussian multiple linear regression model
    event <- predict_event(model, coords, level)[, "date"]
    Q <- stats::quantile(event, probs = c(0.05, 0.95), names = FALSE)
    distrib <- cbind(min(event), Q[1], mean(event), Q[2], max(event))
    return(distrib)
  }

  loop_fun <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbapply
  } else {
    apply
  }
  loop_args <- list(X = x, MARGIN = margin, FUN = compute_stats,
                    n = n, svd = svd, keep = keep, model = model, level = level)
  boot <- do.call(loop_fun, loop_args)

  boot_event <- data.frame(colnames(boot), t(boot), row.names = NULL,
                           stringsAsFactors = FALSE)
  colnames(boot_event) <- c("id", "min", "Q05", "mean", "Q95", "max")
  boot_event
}

#' Jackknife Fabrics
#'
#' @param x An \eqn{m \times p}{m x p} \code{\link{numeric}} matrix of count
#'  data.
#' @param date A list of \code{\link{numeric}} values giving the known dates used
#'  for the linear model fitting.
#' @param model A \code{\link[stats:lm]{linear model}} in which coefficients will
#'  be replaced by the jackknife estimates.
#' @param keep An \code{\link{integer}} giving the number of CA factors to keep
#'  for new data prediction.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param ... Further arguments to be passed to \code{\link[ca]{ca}}.
#' @return A six columns \code{\link{data.frame}} giving the results of
#'  the resamping procedure (jackknifing fabrics) for each assemblage (in rows)
#'  with the following columns:
#'  \describe{
#'   \item{id}{An identifier to link each row to an assemblage.}
#'   \item{date}{The jackknife event date estimate.}
#'   \item{lower}{The lower boundary of the associated prediction interval.}
#'   \item{upper}{The upper boundary of the associated prediction interval.}
#'   \item{error}{The standard error of predicted means.}
#'   \item{bias}{The jackknife estimate of bias.}
#'  }
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
jack_date <- function(x, model, keep = ncol(x), level = 0.95, ...) {
  # Validation
  keep <- as.integer(keep)
  level <- as.numeric(level)

  # Get data
  dates <- rownames_to_column(model$model, factor = TRUE, id = "id")
  dates <- cbind.data.frame(
    id = dates$id,
    date = dates$date
  )

  # CA on the whole dataset
  axes <- min(dim(x))
  results_CA <- ca::ca(x, nd = axes, ...)
  coords_CA <- ca::cacoord(results_CA, type = "principal",
                           rows = TRUE, cols = FALSE)

  compute_coef <- function(i, data, dates, keep, level) {
    # Removing a column may lead to rows filled only with zeros
    # We need to remove such rows to compute CA
    zero <- which(rowSums(data[, -i]) == 0)
    sampled <- if (length(zero) != 0) data[-zero, -i] else data[, -i]
    # Compute CA
    axes <- min(dim(sampled))
    results_CA <- ca::ca(sampled, nd = axes, ...)
    row_coord <- as.data.frame(ca::cacoord(results_CA, type = "principal",
                                           rows = TRUE, cols = FALSE))

    # Gaussian multiple linear regression model
    contexts <- merge(dates, row_coord[, keep], by.x = "id", by.y = "row.names")
    ## Remove column 'id' before fitting
    contexts <- as.data.frame(contexts)
    contexts <- contexts[, !(names(contexts) %in% c("id"))]

    fit <- stats::lm(date ~ ., data = contexts)
    # Return model coefficients
    return(stats::coef(fit))
  }

  loop_fun <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply
  } else {
    lapply
  }
  loop_args <- list(X = seq_len(ncol(x)), FUN = compute_coef,
                    data = x, dates = dates, keep = keep, level = level)
  jack <- do.call(loop_fun, loop_args)

  # Predict event date for each context
  jack_mean <- apply(X = do.call(rbind, jack), MARGIN = 2, FUN = mean)
  jack_fit <- model
  jack_fit$coefficients <- jack_mean
  jack_event <- predict_event(jack_fit, coords_CA, level)
  jack_event
}
