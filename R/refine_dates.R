# REFINE DATE MODEL
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname refine
#' @aliases refine_event,DateModel-method
setMethod(
  f = "refine_event",
  signature = signature(object = "DateModel"),
  definition = function(object, method = c("jackknife", "bootstrap"),
                        level = 0.95, probs = c(0.05, 0.95), n = 1000, ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    # Get data
    fit_model <- object[["model"]]
    fit_data <- object[["data"]]
    fit_dates <- object[["dates"]]
    fit_dim <- object[["dimension"]]

    event <- predict_event(object, level = 0.95)
    ca <- run_ca(arkhe::as_count(fit_data))

    # Check model with resampling methods
    ## Jackknife fabrics
    if ("jackknife" %in% method) {
      jack_coef <- jack_date2(fit_data, fit_dates, cutoff = 100)
      # Change lm coefficients
      fit_model$coefficients <- jack_coef[c(1, fit_dim + 1)]
      # Predict event date for each context
      jack_event <- predict_events(fit_model, ca[["row_coordinates"]], level)
      results <- as.data.frame(jack_event)
      # Compute jaccknife bias
      results$bias <- (ncol(fit_data) - 1) *
        (results$date - event[["row_event"]][, "date"])
    }
    ## Bootstrap assemblages
    if ("bootstrap" %in% method) {
      results <- boot_ca(ca, fun = compute_date_boot, margin = 1, n = n,
                         axes = fit_dim, model = fit_model, level = level,
                         probs = probs)
      results <- do.call(rbind, results)
      results <- as.data.frame(results)
    }
    results
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
  # New CA coordinates
  coords <- x[, axes]
  # Gaussian multiple linear regression model
  event <- predict_events(model, coords, level)[, "date"]
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
#' @param date A \code{\link{numeric}} vector of known dates.
#' @param model A \code{\link[stats:lm]{linear model}} in which coefficients
#'  will be replaced by the jackknife estimates.
#' @param keep An \code{\link{integer}} giving the number of CA factors to keep
#'  for new data prediction.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param ... Further arguments to be passed to \code{\link[ca]{ca}}.
#' @return A \code{\link{numeric}} vector of linear model coefficients.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
jack_date2 <- function(x, dates, cutoff = 90, ...){
  m <- ncol(x)
  k <- seq_len(m)
  jack <- vector(mode = "list", length = m)
  progress_bar <- utils::txtProgressBar(min = 0, max = m, initial = 0,
                                        char = "=", style = 3)
  for (j in k) {
    counts <- arkhe::as_count(x[, -j, drop = FALSE])
    # Removing a column may lead to rows filled only with zeros
    # Suppress warnings from run_ca
    model <- suppressWarnings(
      date_event(counts, dates = dates, cutoff = cutoff)
    )
    jack[[j]] <- stats::coef(model[["model"]]) # Get model coefficients
    utils::setTxtProgressBar(progress_bar, j)
  }
  close(progress_bar)
  jack <- do.call(rbind, jack)
  jack <- apply(X = jack, MARGIN = 2, FUN = mean)
  jack
}
