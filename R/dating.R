#' @include AllGenerics.R AllClasses.R Date-classes.R
NULL

#' @export
#' @rdname date
#' @aliases dateEvent,CountMatrix-method
setMethod(
  f = "dateEvent",
  signature = signature(object = "CountMatrix"),
  definition = function(object, time, axes = 10, level = 0.95, verbose = FALSE, ...) {
    # Validation
    assemblages <- rownames(object)
    dated <- which(assemblages %in% names(time))
    if (length(dated) == 0)
      stop("No matching contexts")
    # Correspondance analysis
    corresp <- FactoMineR::CA(object, ..., ncp = axes, graph = FALSE)
    coord <- as.data.frame(corresp$row$coord)
    # Gaussian multiple linear regression model
    known_contexts <- cbind.data.frame(date = time, coord[dated, ])
    fit <- stats::lm(date ~ ., data = known_contexts)
    if (verbose) print(summary(fit))
    # Predict dates
    predicted <- stats::predict(fit, coord, interval = "confidence", level = level)
    predicted_clean <- as.data.frame(round(predicted, digits = 0))

    methods::new("DateEvent",
                 assemblage = assemblages, level = level,
                 date = as.integer(predicted_clean$fit),
                 earliest = as.integer(predicted_clean$lwr),
                 latest = as.integer(predicted_clean$upr))
  }
)
