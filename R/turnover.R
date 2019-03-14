#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname turnover-method
#' @aliases turnover,CountMatrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), simplify = FALSE, ...) {
    object <- methods::as(object, "IncidenceMatrix")
    B <- turnover(object, method, simplify, ...)
    return(B)
  }
)

#' @export
#' @rdname turnover-method
#' @aliases turnover,IncidenceMatrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), simplify = FALSE, ...) {
    method <- match.arg(method, several.ok = TRUE)
    B <- sapply(X = method, FUN = function(x, data) {
      index <- switch (
        x,
        whittaker = whittakerBeta,
        cody = codyBeta,
        routledge1 = routledge1Beta,
        routledge2 = routledge2Beta,
        routledge3 = routledge3Beta,
        wilson = wilsonBeta
      )
      index(object)
    }, data = object, simplify = simplify)
    return(B)
  }
)

#' Whittaker
#'
#' @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family turnover index
#' @rdname whittaker-index
#' @noRd
whittakerBeta <- function(x) {
  x <- x > 0 # presence/absence

  # Total number of taxa recorded in the system
  S <- sum(colSums(x) > 0)
  # Mean taxa richness
  alpha <- mean(rowSums(x))

  W <- (S / alpha) - 1
  return(W)
}

#' Cody
#'
#' @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
#' @details Begining of the transect in the first row
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family turnover index
#' @rdname cody-index
#' @noRd
codyBeta <- function(x) {
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

#' Routledge's Beta R
#'
#' @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family turnover index
#' @rdname routledge-index
#' @noRd
routledge1Beta <- function(x) {
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
#' @family turnover index
#' @rdname routledge-index
#' @noRd
routledge2Beta <- function(x) {
  x <- x > 0 # presence/absence

  # Number of samples in which each taxa is present
  e <- colSums(x)
  # Taxa richness of each sample
  alpha <- rowSums(x)
  t <- sum(e)

  beta <- log(t) - (1 / t) * sum(e * log(e)) - (1 / t) * sum(alpha * log(alpha))
  return(beta)
}
# Beta E
#' @family turnover index
#' @rdname routledge-index
#' @noRd
routledge3Beta <- function(x) {
  x <- x > 0 # presence/absence

  I <- routledge2Beta(x)
  E <- exp(I)
  return(E)
}

#' Wilson
#'
#' @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
#' @details Begining of the transect in the first row
#' @return A length-one \code{\link{numeric}} vector.
#' @author N. Frerebeau
#' @family turnover index
#' @rdname wilson-index
#' @noRd
wilsonBeta <- function(x) {
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
