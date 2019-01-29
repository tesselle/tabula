#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname date
#' @aliases dateEvent,CountMatrix-method
setMethod(
  f = "dateEvent",
  signature = signature(object = "CountMatrix"),
  definition = function(object, time, axes = 5, level = 0.95, verbose = FALSE, ...) {
    # Validation
    assemblages <- rownames(object)
    dated <- which(assemblages %in% names(time))
    if (length(dated) == 0)
      stop("No matching contexts")

    # Correspondance analysis
    corresp <- FactoMineR::CA(object, ..., ncp = axes, graph = FALSE)
    row_coord <- as.data.frame(corresp$row$coord)
    col_coord <- as.data.frame(corresp$col$coord)

    # Gaussian multiple linear regression model
    contexts <- cbind.data.frame(date = time, row_coord[dated, ])
    fit <- stats::lm(date ~ ., data = contexts)
    fit_summary <- stats::summary.lm(fit)
    if (verbose) print(fit_summary)

    # Predict contexts dates
    row_predict <- stats::predict.lm(fit, row_coord, se.fit = TRUE,
                                     interval = "confidence", level = level)
    row_fit <- cbind(row_predict$fit, error = row_predict$se.fit)
    # TODO: validation !
    colnames(row_fit) <- c("estimation", "earliest", "latest", "error")

    # Predict fabrics dates
    col_fit <- apply(X = row_fit, MARGIN = 2, FUN = function(x, object) {
      # CA transition formulae
      Dc <- diag(colSums(object))
      Phi <- t(matrix(x, nrow = 1))
      solve(Dc) %*% t(object) %*% Phi
    }, object)
    rownames(col_fit) <- colnames(object)

    methods::new("DateEvent",
                 model = fit,
                 level = level,
                 residual = row_predict$residual.scale,
                 rows = row_fit,
                 columns = col_fit)
  }
)
