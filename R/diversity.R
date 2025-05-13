# DIVERSITY
#' @include AllGenerics.R
NULL

# Index ========================================================================
index_observed <- function(x, ...) {
  sum(x > 0, ...)  # Number of observed species
}
get_index <- function(x) {
  match.fun(sprintf("index_%s", x))
}
do_index <- function(x, method, ...) {
  f <- get_index(method)
  f(x, ...)
}
is_evenness <- function(x) {
  methods::is(x, "EvennessIndex")
}

#' Compute a Diversity Index
#'
#' @param x A [`numeric`] [`matrix`].
#' @param method A [`character`] string specifying the measure to be computed.
#' @param by_row A [`logical`] scalar: should `method` be computed for each row?
#' @param ... Further parameters to be passed to `method`.
#' @return A [DiversityIndex-class] object.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
index_diversity <- function(x, method, ..., by_row = TRUE) {
  fun <- get_index(method)
  if (by_row) {
    idx <- apply(X = x, MARGIN = 1, FUN = fun, ...)
  } else {
    idx <- fun(x, ...)
  }

  ## Fix names
  row_names <- rownames(x) %||% paste0("S", seq_along(idx))

  .DiversityIndex(
    idx,
    labels = row_names,
    size = as.integer(rowSums(x)),
    data = x,
    method = method
  )
}

# Diversity ====================================================================
#' @export
#' @rdname diversity
#' @aliases diversity,matrix-method
setMethod(
  f = "diversity",
  signature = c(object = "matrix"),
  definition = function(object, ..., evenness = FALSE, unbiased = FALSE) {

    index <- t(apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, evenness, unbiased) {
        c(
          size = sum(x),
          observed = observed(x),
          ## Heterogeneity
          shannon = index_shannon(x, evenness = evenness, unbiased = unbiased),
          brillouin = index_brillouin(x, evenness = evenness),
          ## Dominance
          simpson = index_simpson(x, evenness = evenness, unbiased = unbiased),
          berger = index_berger(x),
          ## Richness
          menhinick = index_menhinick(x),
          margalef = index_margalef(x),
          chao1 = index_chao1(x, unbiased = unbiased),
          ace = index_ace(x),
          squares = index_squares(x)
        )
      },
      evenness = evenness,
      unbiased = unbiased
    ))
    rownames(index) <- rownames(object)
    as.data.frame(index)
  }
)

#' @export
#' @rdname diversity
#' @aliases diversity,data.frame-method
setMethod(
  f = "diversity",
  signature = c(object = "data.frame"),
  definition = function(object, ..., evenness = FALSE, unbiased = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., evenness = evenness, unbiased = unbiased)
  }
)

# Heterogeneity ================================================================
#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,matrix-method
setMethod(
  f = "heterogeneity",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("shannon", "simpson", "berger",
                                   "boone", "brillouin", "mcintosh")) {
    method <- match.arg(method, several.ok = FALSE)
    by_row <- method != "boone"
    index <- index_diversity(object, method, ..., evenness = FALSE,
                             by_row = by_row)
    .HeterogeneityIndex(index)
  }
)

#' @export
#' @rdname heterogeneity
#' @aliases heterogeneity,data.frame-method
setMethod(
  f = "heterogeneity",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("shannon", "simpson", "berger",
                                   "boone", "brillouin", "mcintosh")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Evenness =====================================================================
#' @export
#' @rdname evenness
#' @aliases evenness,matrix-method
setMethod(
  f = "evenness",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("shannon", "simpson", "brillouin",
                                   "mcintosh")) {
    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method, ..., evenness = TRUE)
    .EvennessIndex(index)
  }
)

#' @export
#' @rdname evenness
#' @aliases evenness,data.frame-method
setMethod(
  f = "evenness",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("shannon", "simpson", "brillouin",
                                   "mcintosh")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Richness =====================================================================
#' @export
#' @rdname richness
#' @aliases richness,matrix-method
setMethod(
  f = "richness",
  signature = c(object = "matrix"),
  definition = function(object, ..., method = c("observed", "margalef", "menhinick")) {
    ## Backward compatibility
    if (method == "count") method <- "observed"

    method <- match.arg(method, several.ok = FALSE)
    index <- index_diversity(object, method, ...)
    .RichnessIndex(index)
  }
)

#' @export
#' @rdname richness
#' @aliases richness,data.frame-method
setMethod(
  f = "richness",
  signature = c(object = "data.frame"),
  definition = function(object, ..., method = c("observed", "margalef", "menhinick")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Composition ==================================================================
#' @export
#' @rdname richness
#' @aliases composition,matrix-method
setMethod(
  f = "composition",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("chao1", "ace", "squares", "chao2", "ice")) {
    method <- match.arg(method, several.ok = FALSE)
    by_row <- any(method == c("chao1", "ace", "squares"))
    index <- index_diversity(object, method, ..., by_row = by_row)
    .CompositionIndex(index)
  }
)

#' @export
#' @rdname richness
#' @aliases composition,data.frame-method
setMethod(
  f = "composition",
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("chao1", "ace", "squares", "chao2", "ice")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)

# Turnover =====================================================================
#' @export
#' @rdname turnover
#' @aliases turnover,matrix-method
setMethod(
  f = "turnover",
  signature = c(object = "matrix"),
  definition = function(object, ...,
                        method = c("whittaker", "cody", "routledge1",
                                   "routledge2", "routledge3", "wilson")) {
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
  signature = c(object = "data.frame"),
  definition = function(object, ...,
                        method = c("whittaker", "cody", "routledge1",
                                   "routledge2", "routledge3", "wilson")) {
    object <- data.matrix(object)
    methods::callGeneric(object, ..., method = method)
  }
)
