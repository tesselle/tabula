# CO-OCCURRENCE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname occurrence
#' @aliases occurrence,matrix-method
setMethod(
  f = "occurrence",
  signature = c(object = "matrix"),
  definition = function(object, method = c("absolute", "relative", "binomial")) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)

    ## Pairwise comparison
    p <- ncol(object)
    labels <- colnames(object)

    if (method == "absolute" || method == "relative") {
      incid <- object > 0
      fun <- function(x) sum(incid[, x[1]] + incid[, x[2]] == 2)
    }
    if (method == "binomial") {
      fun <- function(x) index_binomial(object[, x[1]], object[, x[2]])
    }

    cbn <- utils::combn(p, m = 2, simplify = TRUE)
    index <- apply(X = cbn, MARGIN = 2, FUN = fun)

    ## Matrix of results
    mtx <- matrix(data = 0, nrow = p, ncol = p,
                  dimnames = list(labels, labels))
    mtx[lower.tri(mtx, diag = FALSE)] <- index
    mtx <- t(mtx)
    mtx[lower.tri(mtx, diag = FALSE)] <- index

    if (method == "relative") {
      mtx <- mtx / nrow(object)
    }

    occ <- stats::as.dist(mtx)
    attr(occ, "total") <- nrow(object)
    occ
  }
)

#' @export
#' @rdname occurrence
#' @aliases occurrence,data.frame-method
setMethod(
  f = "occurrence",
  signature = c(object = "data.frame"),
  definition = function(object, method = c("absolute", "relative", "binomial")) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method)
  }
)

## Binomial co-occurrence ------------------------------------------------------
#' @export
#' @rdname index_binomial
#' @aliases index_binomial,numeric,numeric-method
setMethod(
  f = "index_binomial",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    ## Total number of assemblages
    N <- length(x)
    ## Expected proportion of co-occurrences for artifact classes
    p <- sum(x > 0) * sum(y > 0) / N^2
    ## Number of observed co-occurence for artifact classes
    o <- sum((x > 0) + (y > 0) == 2)
    if (p == 1) {
      ## Avoid NaN generation
      ## TODO: warning ?
      Cbi <- 0
    } else {
      Cbi <- (o - N * p) / sqrt(N * p * (1 - p))
    }
    Cbi
  }
)
