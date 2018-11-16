# Turnover measures

# Whittaker ====================================================================
# @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family turnover index
# @rdname whittaker-index
whittakerBeta <- function(x) {
  x <- x > 0 # presence/absence

  # Total number of taxa recorded in the system
  S <- sum(colSums(x) > 0)
  # Mean taxa richness
  alpha <- mean(rowSums(x))

  W <- (S / alpha) - 1
  return(W)
}

# Cody =========================================================================
# @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
# @details Begining of the transect in the first row
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family turnover index
# @rdname cody-index
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

# Routledge ====================================================================
# Beta R -----------------------------------------------------------------------
# @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family turnover index
# @rdname routledge-index
routledge1Beta <- function(x) {
  x <- x > 0 # presence/absence
  p <- ncol(x)

  # Total number of taxa recorded in the system
  S <- sum(colSums(x) > 0)
  # Works out which pairs of taxa occurs together in at least one sample
  combine <- methods::as(x, "OccurrenceMatrix")
  r <- sum(combine) / 2

  beta <- S^2 / (2 * r + S) - 1
  return(beta)
}
# Beta I -----------------------------------------------------------------------
# @rdname routledge-index
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
# Beta E -----------------------------------------------------------------------
# @rdname routledge-index
routledge3Beta <- function(x) {
  x <- x > 0 # presence/absence

  I <- routledge2Beta(x)
  E <- exp(I)
  return(E)
}

# Wilson =======================================================================
# @param x A \code{\link{logical}} or \code{\link{numeric}} matrix.
# @details Begining of the transect in the first row
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family turnover index
# @rdname wilson-index
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
