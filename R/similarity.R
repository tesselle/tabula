# SIMILARITY
#' @include AllGenerics.R
NULL

# Similarity ===================================================================
#' @export
#' @rdname similarity
#' @aliases similarity,matrix-method
setMethod(
  f = "similarity",
  signature = c(object = "matrix"),
  definition = function(object, method = c("brainerd", "bray", "jaccard",
                                           "morisita", "sorenson")) {
    ## Validation
    if (any(method == "binomial")) {
      stop("Use occurrence(x, method = \"bionomial\") instead.", call. = FALSE)
    }
    method <- match.arg(method, several.ok = FALSE)
    ## Select method
    fun <- get_index(method)

    ## Pairwise comparison
    ## Sample/case comparisons
    m <- nrow(object)
    labels <- rownames(object)
    beta <- function(x, f) f(object[x[1], ], object[x[2], ])

    cbn <- utils::combn(seq_len(m), m = 2, simplify = TRUE)
    index <- apply(X = cbn, MARGIN = 2, FUN = beta, f = fun)

    ## Matrix of results
    diag_value <- ifelse(method == "brainerd", 200, 1)
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
  signature = c(object = "data.frame"),
  definition = function(object, method = c("brainerd", "bray", "jaccard",
                                           "morisita", "sorenson")) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method)
  }
)

# Index ========================================================================
## Qualitative index -----------------------------------------------------------
#' @export
#' @rdname index_jaccard
#' @aliases index_jaccard,character,character-method
setMethod(
  f = "index_jaccard",
  signature = c(x = "character", y = "character"),
  definition = function(x, y) {
    inter <- length(intersect(x, y))
    union <- length(x) + length(y) - inter
    inter / union
  }
)

#' @export
#' @rdname index_jaccard
#' @aliases index_jaccard,logical,logical-method
setMethod(
  f = "index_jaccard",
  signature = c(x = "logical", y = "logical"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x)
    b <- sum(y)
    j <- sum((x + y) == 2)
    Cj <- j / (a + b - j)
    Cj
  }
)

#' @export
#' @rdname index_jaccard
#' @aliases index_jaccard,numeric,numeric-method
setMethod(
  f = "index_jaccard",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## presence/absence
    x <- x > 0
    y <- y > 0
    methods::callGeneric(x, y)
  }
)

#' @export
#' @rdname index_sorenson
#' @aliases index_sorenson,logical,logical-method
setMethod(
  f = "index_sorenson",
  signature = c(x = "logical", y = "logical"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x)
    b <- sum(y)
    j <- sum((x + y) == 2)
    Cs <- 2 * j / (a + b)
    Cs
  }
)

#' @export
#' @rdname index_sorenson
#' @aliases index_sorenson,numeric,numeric-method
setMethod(
  f = "index_sorenson",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## presence/absence
    x <- x > 0
    y <- y > 0
    methods::callGeneric(x, y)
  }
)

## Quantitative index ----------------------------------------------------------
#' @export
#' @rdname index_brainerd
#' @aliases index_brainerd,numeric,numeric-method
setMethod(
  f = "index_brainerd",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- x / sum(x)
    b <- y / sum(y)
    Cb <- 2 - sum(abs(a - b))
    Cb * 100
  }
)

#' @export
#' @rdname index_bray
#' @aliases index_bray,numeric,numeric-method
setMethod(
  f = "index_bray",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x) # Number of individuals in site A
    b <- sum(y) # Number of individuals in site B

    Cs <- 1 - sum(abs(y - x)) / sum(x + y)
    Cs
  }
)

#' @export
#' @rdname index_morisita
#' @aliases index_morisita,numeric,numeric-method
setMethod(
  f = "index_morisita",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    a <- sum(x) # Number of individuals in site A
    b <- sum(y) # Number of individuals in site B
    da <- sum(x^2) / a^2
    db <- sum(y^2) / b^2

    Cm <- (2 * sum(x * y)) / ((da + db) * a * b)
    Cm
  }
)
