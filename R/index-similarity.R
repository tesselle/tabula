# Similarity index

# Qualitative index ============================================================
# Jaccard ----------------------------------------------------------------------
# Jaccard similarity index
#
# @param x A length-p \code{\link{numeric}} vector.
# @param y A length-p \code{\link{numeric}} vector.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family similarity index
# @rdname jaccard-index
jaccardSimilarity <- function(x, y) {
  # Validation
  if (length(x) != length(y))
    stop("a and b should have the same length")
  # presence/absence
  x <- x > 0
  y <- y > 0

  a <- sum(x)
  b <- sum(y)
  j <- sum((x + y) == 2)
  Cj <- j / (a + b - j)
  return(Cj)
}

# Sorenson ---------------------------------------------------------------------
# Sorenson similarity index
#
# @param x A length-p \code{\link{numeric}} vector.
# @param y A length-p \code{\link{numeric}} vector.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family similarity index
# @rdname sorenson-index
sorensonSimilarity <- function(x, y) {
  # Validation
  if (length(x) != length(y))
    stop("a and b should have the same length")
  # presence/absence
  x <- x > 0
  y <- y > 0

  a <- sum(x)
  b <- sum(y)
  j <- sum((x + y) == 2)
  Cs <- 2 * j / (a + b)
  return(Cs)
}

# Quantitative index ===========================================================
# Sorenson ---------------------------------------------------------------------
# Sorenson quantitative index
#
# Bray and Curtis modified version of the Sorenson index.
# @param x A length-p \code{\link{numeric}} vector.
# @param y A length-p \code{\link{numeric}} vector.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family similarity index
# @rdname bray-index
braySimilarity <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("numeric values are expected")
  if (length(x) != length(y))
    stop("x and y should have the same length")

  a <- sum(x)
  b <- sum(y)

  j <- sum(apply(X = rbind(x, y), MARGIN = 2, FUN = min))
  Cs <- 2 * j / (a + b)
  return(Cs)
}

# Morisita-Horn ----------------------------------------------------------------
# Morisita-Horn quantitative index
#
# @param x A length-p \code{\link{numeric}} vector.
# @param y A length-p \code{\link{numeric}} vector.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family similarity index
# @rdname morisita-index
morisitaSimilarity <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("numeric values are expected")
  if (length(x) != length(y))
    stop("x and y should have the same length")

  a <- sum(x) # Number of individuals in site A
  b <- sum(y) # Number of individuals in site B
  da <- sum(x^2) / a^2
  db <- sum(y^2) / b^2

  Cm <- (2 * sum(x * y)) / ((da + db) * a * b)
  return(Cm)
}

# Brainerd-Robinson ------------------------------------------------------------
# Brainerd-Robinson quantitative index
#
# @param x A length-p \code{\link{numeric}} vector.
# @param y A length-p \code{\link{numeric}} vector.
# @return A length-one \code{\link{numeric}} vector.
# @author N. Frerebeau
# @family similarity index
# @rdname brainerd-index
brainerdSimilarity <- function(x, y) {
  # Validation
  if (!is.numeric(x) | !is.numeric(y))
    stop("numeric values are expected")
  if (length(x) != length(y))
    stop("x and y should have the same length")

  a <- x / sum(x)
  b <- y / sum(y)
  Cb <- 2 - sum(abs(a - b))
  return(Cb * 100)
}
