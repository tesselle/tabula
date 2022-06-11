# SIMILARITY
#' @include AllGenerics.R AllClasses.R
NULL

# Similarity ===================================================================
#' @export
#' @rdname similarity
#' @aliases similarity,matrix-method
setMethod(
  f = "similarity",
  signature = signature(object = "matrix"),
  definition = function(object, method = c("brainerd", "bray", "jaccard",
                                           "morisita", "sorenson",
                                           "binomial")) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)
    ## Select method
    fun <- get_index(method)

    ## Pairwise comparison
    by_row <- method != "binomial"
    if (by_row) {
      ## Sample/case comparisons
      m <- nrow(object)
      labels <- rownames(object)
      beta <- function(x, f) f(object[x[1], ], object[x[2], ])
    } else {
      ## Taxa/type comparisons
      m <- ncol(object)
      labels <- colnames(object)
      beta <- function(x, f) f(object[, x[1]], object[, x[2]])
    }

    diag_value <- switch (
      method,
      brainerd = 200,
      binomial = 0,
      1
    )

    cbn <- utils::combn(seq_len(m), 2)
    index <- apply(X = cbn, MARGIN = 2, FUN = beta, f = fun)

    ## Matrix of results
    mtx <- matrix(data = diag_value, nrow = m, ncol = m,
                  dimnames = list(labels, labels))
    mtx[lower.tri(mtx, diag = FALSE)] <- index
    mtx <- t(mtx)
    mtx[lower.tri(mtx, diag = FALSE)] <- index

    sim <- stats::as.dist(mtx)
    attr(sim, "method") <- method
    sim
  }
)

#' @export
#' @rdname similarity
#' @aliases similarity,data.frame-method
setMethod(
  f = "similarity",
  signature = signature(object = "data.frame"),
  definition = function(object, method = c("brainerd", "bray", "jaccard",
                                           "morisita", "sorenson",
                                           "binomial")) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method)
  }
)

# Co-occurrence ================================================================
#' @export
#' @rdname occurrence
#' @aliases occurrence,matrix-method
setMethod(
  f = "occurrence",
  signature = signature(object = "matrix"),
  definition = function(object) {
    incid <- object > 0
    m <- nrow(incid)
    p <- ncol(incid)

    ij <- utils::combn(p, m = 2, simplify = TRUE)
    pair <- seq_len(ncol(ij))

    mtx <- matrix(data = 0L, nrow = p, ncol = p)
    labels <- colnames(incid)
    dimnames(mtx) <- list(labels, labels)

    for (k in pair) {
      i <- ij[1, k]
      j <- ij[2, k]
      z <- as.integer(sum(incid[, i] + incid[, j] == 2))
      mtx[i, j] <- mtx[j, i] <- z
    }

    occ <- stats::as.dist(mtx)
    attr(occ, "total") <- m
    occ
  }
)

#' @export
#' @rdname occurrence
#' @aliases occurrence,data.frame-method
setMethod(
  f = "occurrence",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

# Index ========================================================================
## Qualitative index -----------------------------------------------------------
#' @export
#' @rdname similarity
#' @aliases index_jaccard,character,character-method
setMethod(
  f = "index_jaccard",
  signature = signature(x = "character", y = "character"),
  definition = function(x, y) {
    inter <- length(intersect(x, y))
    union <- length(x) + length(y) - inter
    inter / union
  }
)

#' @export
#' @rdname similarity
#' @aliases index_jaccard,logical,logical-method
setMethod(
  f = "index_jaccard",
  signature = signature(x = "logical", y = "logical"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x)
    b <- sum(y)
    j <- sum((x + y) == 2)
    Cj <- j / (a + b - j)
    return(Cj)
  }
)

#' @export
#' @rdname similarity
#' @aliases index_jaccard,numeric,numeric-method
setMethod(
  f = "index_jaccard",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## presence/absence
    x <- x > 0
    y <- y > 0
    methods::callGeneric(x, y)
  }
)

#' @export
#' @rdname similarity
#' @aliases index_sorenson,logical,logical-method
setMethod(
  f = "index_sorenson",
  signature = signature(x = "logical", y = "logical"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x)
    b <- sum(y)
    j <- sum((x + y) == 2)
    Cs <- 2 * j / (a + b)
    return(Cs)
  }
)

#' @export
#' @rdname similarity
#' @aliases index_sorenson,numeric,numeric-method
setMethod(
  f = "index_sorenson",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## presence/absence
    x <- x > 0
    y <- y > 0
    methods::callGeneric(x, y)
  }
)

## Quantitative index ----------------------------------------------------------
#' @export
#' @rdname similarity
#' @aliases index_bray,numeric,numeric-method
setMethod(
  f = "index_bray",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x)
    b <- sum(y)

    j <- sum(apply(X = rbind(x, y), MARGIN = 2, FUN = min))
    Cs <- 2 * j / (a + b)
    return(Cs)
  }
)

#' @export
#' @rdname similarity
#' @aliases index_morisita,numeric,numeric-method
setMethod(
  f = "index_morisita",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x) # Number of individuals in site A
    b <- sum(y) # Number of individuals in site B
    da <- sum(x^2) / a^2
    db <- sum(y^2) / b^2

    Cm <- (2 * sum(x * y)) / ((da + db) * a * b)
    return(Cm)
  }
)

#' @export
#' @rdname similarity
#' @aliases index_brainerd,numeric,numeric-method
setMethod(
  f = "index_brainerd",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_count(x)
    arkhe::assert_count(y)
    arkhe::assert_length(y, length(x))

    a <- x / sum(x)
    b <- y / sum(y)
    Cb <- 2 - sum(abs(a - b))
    return(Cb * 100)
  }
)

## Binomial co-occurrence ------------------------------------------------------
#' @export
#' @rdname similarity
#' @aliases index_binomial,numeric,numeric-method
setMethod(
  f = "index_binomial",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_count(x)
    arkhe::assert_count(y)
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
    return(Cbi)
  }
)
