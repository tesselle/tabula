#' @include AllGenerics.R AllClasses.R date.R
NULL

# @param x An \eqn{m \times p}{m x p} data matrix.
# @param cutoff A function that takes a numeric vector as argument and returns
#  a single numeric value.
# @param margin A non-negative \code{\link{integer}} giving the subscripts
#  which the rearrangement will be applied over: \code{1} indicates rows,
#  \code{2} indicates columns.
# @param n A non-negative \code{\link{integer}} giving the number of partial
#  bootstrap replications.
# @param axes A \code{\link{numeric}} vector giving the subscripts of the CA
#  components to use.
# @return A list of \code{\link[=data.frame]{data frames}} giving the convex
#  hull coordinates for each replicated sample.
# @author N. Frerebeau
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

# @param x An \eqn{m \times p}{m x p} \code{\link{numeric}} matrix of count data.
# @param model
# @param margin A non-negative \code{\link{integer}} giving the subscripts
#  which the rearrangement will be applied over: \code{1} indicates rows,
#  \code{2} indicates columns.
# @param n A non-negative \code{\link{integer}} giving the number of partial
#  bootstrap replications.
# @param keep An \code{\link{integer}} giving the number of CA factors to keep
#  for new data prediction.
# @param level A length-one \code{\link{numeric}} vector giving the
#  confidence level.
# @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
# @return A six columns \code{\link{data.frame}} giving the boostrap
#  distribution statistics for each replicated sample (in rows) with the
#  following columns:
#  \describe{
#   \item{id}{An identifier.}
#   \item{min}{Minimum value.}
#   \item{Q05}{Sample quantile to 0.05 probability.}
#   \item{mean}{Mean value.}
#   \item{Q95}{Sample quantile to 0.95 probability.}
#   \item{max}{Maximum value.}
#  }
# @author N. Frerebeau
bootDate <- function(x, model, margin = 1, n = 1000, keep = ncol(x),
                     level = 0.95, ...) {
  # Validation
  margin <- as.integer(margin)
  n <- as.integer(n)
  keep <- as.integer(keep)
  level <- as.numeric(level)

  # CA on the whole dataset
  axes <- min(dim(x))
  results_CA <- FactoMineR::CA(x, ncp = axes, graph = FALSE, ...)
  svd <- if (margin == 1) results_CA$svd$V else results_CA$svd$U

  # Compute date event statistics for each replicated sample
  computeStats <- function(x, n, svd, keep, model) {
    # n random replicates
    # replicated <- sample(x = n, size = sum(x), prob = x)
    replicated <- stats::rmultinom(n = n, size = sum(x), prob = x)
    # Compute new CA coordinates
    coords <- crossprod(replicated / colSums(replicated), svd)
    coords <- coords[, keep]
    # Workaround: same colnames as FactoMineR results used to build the model
    colnames(coords) <- paste("Dim", keep, sep = " ")
    # Gaussian multiple linear regression model
    event <- predictEvent(model, coords, level)$fit[, "estimation"]
    Q <- stats::quantile(event, probs = c(0.05, 0.95), names = FALSE)
    distrib <- cbind(min(event), Q[1], mean(event), Q[2], max(event))
    return(distrib)
  }

  boot <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbapply(X = x, MARGIN = margin,
                     FUN = computeStats,
                     n = n, svd = svd, keep = keep, model = model)
  } else {
    apply(X = x, MARGIN = margin,
          FUN = computeStats,
          n = n, svd = svd, keep = keep, model = model)
  }

  boot_event <- data.frame(colnames(boot), t(boot), row.names = NULL)
  colnames(boot_event) <- c("id", "min", "Q05", "mean", "Q95", "max")
  return(boot_event)
}

# @param x An \eqn{m \times p}{m x p} \code{\link{numeric}} matrix of count
#  data.
# @param date A list of \code{\link{numeric}} values giving the known dates used
#  for the linear model fitting.
# @param model A \code{\link[stats:lm]{linear model}} in which coefficients will
#  be replaced by the jackknife estimates.
# @param keep An \code{\link{integer}} giving the number of CA factors to keep
#  for new data prediction.
# @param level A length-one \code{\link{numeric}} vector giving the
#  confidence level.
# @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
# @return A five columns \code{\link{data.frame}} giving the predicted
#  event dates, the corresponding confidence interval boundaries, standard
#  error of the predicted dates and an identifier.
# @author N. Frerebeau
jackDate <- function(x, dates, model, keep = ncol(x), level = 0.95, ...) {
  # Validation
  keep <- as.integer(keep)
  level <- as.numeric(level)

  # CA on the whole dataset
  axes <- min(dim(x))
  results_CA <- FactoMineR::CA(x, ncp = axes, graph = FALSE, ...)

  computeCoef <- function(i, data, dates, keep, level) {
    # Removing a column may lead to rows filled only with zeros
    # We need to remove such rows to compute CA
    zero <- which(rowSums(data[, -i]) == 0)
    sampled <- if (length(zero) != 0) data[-zero, -i] else data[, -i]
    # Compute CA
    axes <- min(dim(sampled))
    results_CA <- FactoMineR::CA(sampled, ncp = axes, graph = FALSE, ...)
    row_coord <- as.data.frame(results_CA$row$coord)
    # Gaussian multiple linear regression model
    contexts <- merge(dates, row_coord[, keep], by.x = "id", by.y = "row.names")
    contexts %<>% as.data.frame() %>% dplyr::select(-1)
    fit <- stats::lm(date ~ ., data = contexts)
    # Return model coefficients
    return(stats::coef(fit))
  }

  jack_coef <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply(X = 1:ncol(x), FUN = computeCoef,
                     data = x, dates = dates, keep = keep, level = level)
  } else {
    lapply(X = 1:ncol(x), FUN = computeCoef,
           data = x, dates = dates, keep = keep, level = level)
  }

  # Predict event date for each context
  jack_mean <- apply(X = do.call(rbind, jack_coef), MARGIN = 2, FUN = mean)
  jack_fit <- model
  jack_fit$coefficients <- jack_mean
  jack_event <- predictEvent(jack_fit, results_CA$row$coord, level)$fit

  return(jack_event)
}

# Refine matrix seriation ======================================================
#' @export
#' @describeIn refine Performs a partial bootstrap correspondance analysis
#'  seriation refinement.
#'  \code{refine} allows to identify samples that are subject to sampling error
#'  or samples that have underlying structural relationships and might be
#'  influencing the ordering along the CA space.
#'  This relies on a partial bootstrap approach to CA-based seriation where each
#'  sample is replicated \code{n} times. The maximum dimension length of
#'  the convex hull around the sample point cloud allows to remove samples for
#'  a given \code{cutoff} value.
#'
#'  According to Peebles and Schachner (2012), "[this] point removal procedure
#'  [results in] a reduced dataset where the position of individuals within the
#'  CA are highly stable and which produces an ordering consistend with the
#'  assumptions of frequency seriation."
#'
#'  \code{refine} returns a \linkS4class{BootCA} object containing the subscript
#'  of samples to be kept (i.e. samples with maximum dimension length of the
#'  convex hull smaller than the cutoff value).
#' @aliases refine,CountMatrix-method
setMethod(
  f = "refine",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2), ...) {
    # Partial bootstrap CA
    hull_rows <- bootHull(object, n = n, margin = 1, axes = axes, ...)
    hull_columns <- bootHull(object, n = n, margin = 2, axes = axes, ...)
    # Get convex hull maximal dimension length for each sample
    hull_length <- sapply(X = hull_rows, function(x) {
      max(stats::dist(x, method = "euclidean"))
    })
    lengths <- data.frame(id = names(hull_length), d = hull_length,
                          row.names = seq_along(hull_length))
    # Get cutoff value
    limit <- cutoff(hull_length)
    # Samples to be kept
    keep <- which(hull_length < limit)
    # Bind hull vertices in a data.frame
    rows <- dplyr::bind_rows(hull_rows, .id = "id")
    cols <- dplyr::bind_rows(hull_columns, .id = "id")

    methods::new("BootCA",
                 rows = rows, columns = cols, cutoff = limit,
                 lengths = lengths, keep = keep)
  }
)

# Refine date model ============================================================
#' @export
#' @describeIn refine TODO.
#' @aliases refine,DateModel-method
setMethod(
  f = "refine",
  signature = signature(object = "DateModel"),
  definition = function(object, n = 1000, jackknife = TRUE,
                        bootstrap = TRUE, ...) {
    # Get data
    counts <- object@counts
    dates <- object@dates
    fit <- object@model
    level <- object@level
    row_dates <- object@rows$estimation
    n_dim <- length(stats::coef(fit)) - 1
    n_col <- ncol(counts)

    jack_event <- boot_event <- NULL
    # Jackknife fabrics
    if (jackknife) {
      jack_event <- jackDate(counts, dates, fit, keep = 1:n_dim,
                             level = level, ...)
      jack_results <- cbind.data.frame(
        jack_event,
        bias = (n_col - 1) * (jack_event$estimation - row_dates)
      )
    }
    # Bootstrap assemblages
    if (bootstrap) {
      boot_event <- bootDate(counts, fit, margin = 1, n = n,
                             keep = 1:n_dim, level = level, ...)
    }

    methods::new("BootDate", jackknife = jack_results, bootstrap = boot_event)
  }
)
