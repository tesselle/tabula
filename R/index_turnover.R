#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname turnover
#' @aliases turnover,matrix-method
setMethod(
  f = "turnover",
  signature = signature(object = "matrix"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), ...) {
    method <- match.arg(method, several.ok = FALSE)
    fun <- get_index(method)
    fun(object)
  }
)

#' @export
#' @rdname turnover
#' @aliases turnover,data.frame-method
setMethod(
  f = "turnover",
  signature = signature(object = "data.frame"),
  definition = function(object, method = c("whittaker", "cody", "routledge1",
                                           "routledge2", "routledge3",
                                           "wilson"), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, method = method)
  }
)

# ==============================================================================

#' @export
#' @rdname turnover
#' @aliases index_whittaker,matrix-method
setMethod(
  f = "index_whittaker",
  signature = signature(x = "matrix"),
  definition = function(x) {
    x <- x > 0 # presence/absence

    # Total number of taxa recorded in the system
    S <- sum(colSums(x) > 0)
    # Mean taxa richness
    alpha <- mean(rowSums(x))

    W <- (S / alpha) - 1
    return(W)
  }
)

#' @export
#' @rdname turnover
#' @aliases index_cody,matrix-method
setMethod(
  f = "index_cody",
  signature = signature(x = "matrix"),
  definition = function(x) {
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
)

#' @export
#' @rdname turnover
#' @aliases index_routledge1,matrix-method
setMethod(
  f = "index_routledge1",
  signature = signature(x = "matrix"),
  definition = function(x) {
    # Beta R
    x <- x > 0 # presence/absence
    p <- ncol(x)

    # Total number of taxa recorded in the system
    S <- sum(colSums(x) > 0)
    # Works out which pairs of taxa occurs together in at least one sample
    combine <- as.matrix(occurrence(x)) > 0
    r <- sum(combine) / 2

    beta <- S^2 / (2 * r + S) - 1
    return(beta)
  }
)

#' @export
#' @rdname turnover
#' @aliases index_routledge2,matrix-method
setMethod(
  f = "index_routledge2",
  signature = signature(x = "matrix"),
  definition = function(x) {
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
)

#' @export
#' @rdname turnover
#' @aliases index_routledge3,matrix-method
setMethod(
  f = "index_routledge3",
  signature = signature(x = "matrix"),
  definition = function(x) {
    # Beta E
    I <- index_routledge2(x)
    E <- exp(I)
    return(E)
  }
)

#' @export
#' @rdname turnover
#' @aliases index_wilson,matrix-method
setMethod(
  f = "index_wilson",
  signature = signature(x = "matrix"),
  definition = function(x) {
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
)
