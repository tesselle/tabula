#' @include AllGenerics.R AllClasses.R
NULL

# Shannon ======================================================================
variance_shannon <- function(x, base = exp(1), na.rm = FALSE, ...) {
  ## Validation
  x <- x[x > 0] # Remove unobserved species
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  S <- length(x)
  p <- x / N

  a <- sum(p * (log(p, base = base))^2)
  b <- sum(p * log(p, base = base))^2

  var <- ((a - b) / N) + ((S - 1) / (2 * N^2))
  var
}

#' @export
#' @rdname test
#' @aliases test_shannon,numeric,numeric-method
setMethod(
  f = "test_shannon",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, ...) {
    ## Validation
    arkhe::assert_length(y, length(x))

    ## Calculate the number of individuals
    Nx <- sum(x, na.rm = TRUE)
    Ny <- sum(y, na.rm = TRUE)

    ## Calculate Shannon diversity
    ## See PAST documentation, p. 208
    d <- (sum(x > 0) - 1) / (2 * sum(x))
    Hx <- index_shannon(x, ...)
    Hy <- index_shannon(y, ...)

    ## Calculate Shannon variance
    Vx <- variance_shannon(x, ...)
    Vy <- variance_shannon(y, ...)

    ## t test statistic
    tt <- (Hx - Hy) / sqrt(Vx + Vy)

    ## Degrees of freedom
    df <- (Vx + Vy)^2 / sum(c(Vx, Vy)^2 / c(Nx, Ny))

    ## p value
    p <- 2 * (1 - stats::pt(q = abs(tt), df = df))

    list(
      statistic = tt,
      parameter = df,
      p.value = p
    )
  }
)

#' @export
#' @describeIn test Produces two sided pairwise comparisons.
#' @aliases test_shannon,matrix,missing-method
setMethod(
  f = "test_shannon",
  signature = c(x = "matrix", y = "missing"),
  definition = function(x, adjust = "holm", ...) {
    ## Get the names of the assemblages
    row_names <- rownames(x)
    if (length(row_names) != 0) {
      row_names <- factor(row_names, levels = unique(row_names))
    } else {
      row_names <- factor(seq_len(nrow(x)))
    }

    ## Compute t test
    compare <- function(i, j) {
      test_shannon(x[i, ], x[j, ], ...)$p.value
    }

    result <- stats::pairwise.table(
      compare.levels = compare,
      level.names = row_names,
      p.adjust.method = adjust
    )
    result
  }
)

#' @export
#' @describeIn test Produces two sided pairwise comparisons.
#' @aliases test_shannon,data.frame,missing-method
setMethod(
  f = "test_shannon",
  signature = c(x = "data.frame", y = "missing"),
  definition = function(x, adjust = "holm", ...) {
    x <- data.matrix(x)
    methods::callGeneric(x, adjust = adjust, ...)
  }
)

# Simpson ======================================================================
variance_simpson <- function(x, na.rm = FALSE, ...) {
  ## Validation
  x <- x[x > 0] # Remove unobserved species
  if (na.rm) x <- stats::na.omit(x) # Remove NAs
  if (anyNA(x)) return(NA)

  N <- sum(x)
  S <- length(x)
  p <- x / N

  a <- 4 * N * (N - 1) * (N - 2) * sum(p^3)
  b <- 2 * N * (N - 1) * sum(p^2)
  c <- 2 * N * (N - 1) * (2 * N - 3) * sum(p^2)^2

  var <- (a + b - c) / (N^2 * (N - 1)^2)
  var
}

#' @export
#' @rdname test
#' @aliases test_simpson,numeric,numeric-method
setMethod(
  f = "test_simpson",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, adjust = "holm", ...) {
    ## Validation
    arkhe::assert_length(y, length(x))

    ## Calculate the number of individuals
    Nx <- sum(x, na.rm = TRUE)
    Ny <- sum(y, na.rm = TRUE)

    ## Calculate Shannon diversity
    Hx <- index_simpson(x, ...)
    Hy <- index_simpson(y, ...)

    ## Calculate Shannon variance
    Vx <- variance_simpson(x, ...)
    Vy <- variance_simpson(y, ...)

    ## t test statistic
    tt <- (Hx - Hy) / sqrt(Vx + Vy)

    ## Degrees of freedom
    df <- (Vx + Vy)^2 / sum(c(Vx, Vy)^2 / c(Nx, Ny))

    ## p value
    p <- 2 * (1 - stats::pt(q = abs(tt), df = df))

    list(
      statistic = tt,
      parameter = df,
      p.value = p
    )
  }
)

#' @export
#' @describeIn test Produces two sided pairwise comparisons.
#' @aliases test_simpson,matrix,missing-method
setMethod(
  f = "test_simpson",
  signature = c(x = "matrix", y = "missing"),
  definition = function(x, adjust = "holm", ...) {
    ## Get the names of the assemblages
    row_names <- rownames(x)
    if (length(row_names) != 0) {
      row_names <- factor(row_names, levels = unique(row_names))
    } else {
      row_names <- factor(seq_len(nrow(x)))
    }

    ## Compute t test
    compare <- function(i, j) {
      test_simpson(x[i, ], x[j, ])$p.value
    }

    result <- stats::pairwise.table(
      compare.levels = compare,
      level.names = row_names,
      p.adjust.method = adjust
    )
    result
  }
)

#' @export
#' @describeIn test Produces two sided pairwise comparisons.
#' @aliases test_simpson,data.frame,missing-method
setMethod(
  f = "test_simpson",
  signature = c(x = "data.frame", y = "missing"),
  definition = function(x, adjust = "holm", ...) {
    x <- data.matrix(x)
    methods::callGeneric(x, adjust = adjust)
  }
)
