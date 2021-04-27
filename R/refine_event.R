# REFINE DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname event
#' @aliases jackknife_event,DateEvent-method
setMethod(
  f = "jackknife_event",
  signature = signature(object = "DateEvent"),
  definition = function(object, level = 0.95,
                        progress = getOption("tabula.progress"), ...) {
    ## Get data
    fit_model <- object[["model"]]
    fit_data <- object[["data"]]
    fit_dates <- object[["dates"]]
    fit_dim <- object[["dimension"]]

    event <- predict_event(object, level = level)

    ## TODO: check cutoff value
    jack_values <- compute_date_jack(fit_data, fit_dates, cutoff = 150,
                                   progress = progress)
    jack_coef <- colMeans(jack_values)

    ## Change lm coefficients
    fit_model$coefficients <- jack_coef[c(1, fit_dim + 1)]

    ## Predict event date for each context
    results_CA <- dimensio::ca(fit_data, ...)
    row_coord <- dimensio::get_coordinates(results_CA, margin = 1)

    jack_event <- compute_event(fit_model, row_coord, level = level)
    results <- as.data.frame(jack_event)

    results$bias <- (ncol(fit_data) - 1) * (results$date - event$date)

    return(results)
  }
)

#' @export
#' @rdname event
#' @aliases bootstrap_event,DateEvent-method
setMethod(
  f = "bootstrap_event",
  signature = signature(object = "DateEvent"),
  definition = function(object, level = 0.95, probs = c(0.05, 0.95), n = 1000,
                        progress = getOption("tabula.progress"), ...) {
    ## Get data
    fit_model <- object[["model"]]
    fit_dim <- object[["dimension"]]

    ## Partial bootstrap CA
    ca_res <- dimensio::bootstrap(object, n = n)
    ca_rep_row <- dimensio::get_replications(ca_res, margin = 1)

    ## Bootstrap assemblages
    event_rows <- apply(
      X = ca_rep_row,
      MARGIN = 1,
      FUN = function(x, axes, model, level, probs) {
        compute_date_boot(t(x), axes, model, level, probs)
      },
      axes = fit_dim,
      model = fit_model,
      level = level,
      probs = probs
    )
    as.data.frame(t(event_rows))
  }
)

#' Bootstrap Resampling of Assemblages
#'
#' Computes date event bootstraped statistics for each replicated sample.
#' @param x A \code{\link{numeric}} matrix of bootstrap replicates.
#' @param axes A \code{\link{numeric}} vector giving the subscripts
#'  of the CA components to keep.
#' @param model An object of class \code{\link[stats]{lm}}.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param probs A \code{\link{numeric}} vector of probabilities with values in
#'  \eqn{[0,1]} (see \code{\link[stats:quantile]{quantile}}).
#' @return A \code{\link{numeric}} vector with the following elements:
#'  \describe{
#'   \item{id}{An identifier to link each row to an assemblage.}
#'   \item{min}{Minimum value.}
#'   \item{mean}{Mean value (event date).}
#'   \item{max}{Maximum value.}
#'   \item{Q05}{Sample quantile to 0.05 probability.}
#'   \item{Q95}{Sample quantile to 0.95 probability.}
#'  }
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_date_boot <- function(x, axes, model, level, probs = c(0.05, 0.95)) {
  ## Remove missing values
  coords <- stats::na.omit(x[, axes])
  ## Gaussian multiple linear regression model
  event <- compute_event(model, coords, level)[, 1]
  Q <- stats::quantile(event, probs = probs, names = FALSE)

  distrib <- c(min(event), mean(event), max(event), Q)
  quant <- paste0("Q", round(probs * 100, 0))
  names(distrib) <- c("min", "mean", "max", quant)
  return(distrib)
}

#' Jackknife Fabrics
#'
#' Compute date event jackknifed statistics for each replicated sample.
#' @param x A \code{\link{numeric}} matrix of count data.
#' @param dates A \code{\link{numeric}} vector of known dates.
#' @param cutoff A \code{\link{numeric}} value.
#' @param progress A \code{\link{logical}} scalar: should a progress bar be
#'  displayed?
#' @param ... Further arguments to be passed to \code{\link[ca]{ca}}.
#' @return A \code{\link{numeric}} vector of linear model coefficients.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
compute_date_jack <- function(x, dates, cutoff = 90,
                              progress = getOption("tabula.progress"), ...) {
  m <- ncol(x)
  k <- seq_len(m)
  jack <- vector(mode = "list", length = m)

  progress_bar <- interactive() && progress
  if (progress_bar) pbar <- utils::txtProgressBar(max = m, style = 3)

  for (j in k) {
    counts <- x[, -j, drop = FALSE]
    ## Removing a column may lead to rows filled only with zeros
    ## TODO: warning
    if (any(rowSums(counts) == 0)) next
    model <- date_event(counts, dates = dates, cutoff = cutoff)
    jack[[j]] <- stats::coef(model[["model"]]) # Get model coefficients
    if (progress_bar) utils::setTxtProgressBar(pbar, j)
  }

  if (progress_bar) close(pbar)
  do.call(rbind, jack)
}
