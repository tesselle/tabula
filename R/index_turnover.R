#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname turnover-index
#' @aliases turnover,CountMatrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), simplify = FALSE, ...) {
    object <- arkhe::as_incidence(object)
    turnover(object, method, simplify, ...)
  }
)

#' @export
#' @rdname turnover-index
#' @aliases turnover,IncidenceMatrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), simplify = FALSE, ...) {
    method <- match.arg(method, several.ok = TRUE)
    mtx <- as.matrix(object)

    B <- lapply(
      X = method,
      FUN = function(x, data) {
        index <- switch (
          x,
          whittaker = turnoverWhittaker,
          cody = turnoverCody,
          routledge1 = turnoverRoutledge1,
          routledge2 = turnoverRoutledge2,
          routledge3 = turnoverRoutledge3,
          wilson = turnoverWilson
        )
        index(object)
      },
      data = mtx
    )
    names(B) <- method
    if (simplify) B <- simplify2array(B, higher = FALSE)
    return(B)
  }
)

# ==============================================================================
#' Turnover index
#'
#' @param x A [`logical`] or [`numeric`] matrix.
#' @return A length-one [`numeric`] vector.
#' @author N. Frerebeau
#' @family diversity measures
#' @name index-turnover
#' @keywords internal
#' @noRd

# @rdname index-turnover
turnoverWhittaker <- function(x) {
  x <- x > 0 # presence/absence
  #' @name index-turnover

  # Total number of taxa recorded in the system
  S <- sum(colSums(x) > 0)
  # Mean taxa richness
  alpha <- mean(rowSums(x))

  W <- (S / alpha) - 1
  return(W)
}

# @rdname index-turnover
turnoverCody <- function(x) {
  x <- x > 0 # presence/absence
  m <- nrow(x)
  first_transect <- x[1, ]
  last_transect <- x[m, ]

  # Number of taxa gained along the transect
  absent <- x[, !first_transect] # Keep only taxa absent in the first transect
  gained <- sum(apply(X = absent, MARGIN = 2, FUN = cumsum)[m, ] > 0)
  # Number of taxa lost
  lost <- sum(!last_transect)

  beta <- (gained + lost) / 2
  return(beta)
}

# @rdname index-turnover
turnoverRoutledge1 <- function(x) {
  # Beta R
  x <- x > 0 # presence/absence
  p <- ncol(x)

  # Total number of taxa recorded in the system
  S <- sum(colSums(x) > 0)
  # Works out which pairs of taxa occurs together in at least one sample
  combine <- methods::as(x, "OccurrenceMatrix") > 0
  r <- sum(combine) / 2

  beta <- S^2 / (2 * r + S) - 1
  return(beta)
}
# @rdname index-turnover
turnoverRoutledge2 <- function(x) {
  # Beta I
  x <- x > 0 # presence/absence

  # Number of samples in which each taxa is present
  e <- colSums(x)
  # Taxa richness of each sample
  alpha <- rowSums(x)
  t <- sum(e)

  beta <- log(t) - (1 / t) * sum(e * log(e)) - (1 / t) * sum(alpha * log(alpha))
  return(beta)
}
# @rdname index-turnover
turnoverRoutledge3 <- function(x) {
  # Beta E
  I <- turnoverRoutledge2(x)
  E <- exp(I)
  return(E)
}

# @rdname index-turnover
turnoverWilson <- function(x) {
  x <- x > 0 # presence/absence
  m <- nrow(x)
  first_transect <- x[1, ]
  last_transect <- x[m, ]

  # Mean taxa richness
  alpha <- mean(rowSums(x))
  # Number of taxa gained along the transect
  absent <- x[, !first_transect] # Keep only taxa absent in the first transect
  gained <- sum(apply(X = absent, MARGIN = 2, FUN = cumsum)[m, ] > 0)
  # Number of taxa lost
  lost <- sum(!last_transect)

  beta <- (gained + lost) / (2 * alpha)
  return(beta)
}
