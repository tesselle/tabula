# RAREFACTION
#' @include AllGenerics.R
NULL

#' @export
#' @rdname rarefaction
#' @aliases rarefaction,matrix-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "matrix"),
  definition = function(object, sample = NULL, method = c("hurlbert", "baxter"),
                        step = 1) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)

    n <- nrow(object)
    if (is.null(sample)) {
      sample <- rowSums(object)
    }
    if (length(sample) == 1) {
      sample <- rep(sample, n)
    }
    k <- seq(from = 1, to = max(sample), by = step)

    ## Matrix of results
    z <- matrix(data = NA_real_, nrow = n, ncol = length(k))
    row_names <- rownames(object) %||% paste0("S", seq_len(n)) # Fix names
    dimnames(z) <- list(row_names, k)

    for (i in seq_len(n)) {
      spl <- k[k <= sample[[i]]]
      rare <- vapply(
        X = spl,
        FUN = function(s, x, f) f(x, s),
        FUN.VALUE = numeric(1),
        x = object[i, ],
        f = get_index(method) # Select method
      )
      z[i, seq_along(rare)] <- rare
    }

    .RarefactionIndex(
      z,
      labels = row_names,
      size = as.integer(k),
      method = method
    )
  }
)

#' @export
#' @rdname rarefaction
#' @aliases rarefaction,data.frame-method
setMethod(
  f = "rarefaction",
  signature = signature(object = "data.frame"),
  definition = function(object, sample = NULL, method = c("hurlbert", "baxter"),
                        step = 1) {
    object <- data.matrix(object)
    methods::callGeneric(object, sample = sample, method = method, step = step)
  }
)

# Plot =========================================================================
#' @export
#' @method plot RarefactionIndex
plot.RarefactionIndex <- function(x, color = NULL, symbol = FALSE,
                                  xlab = NULL, ylab = NULL,
                                  main = NULL, sub = NULL,
                                  ann = graphics::par("ann"),
                                  axes = TRUE, frame.plot = axes,
                                  panel.first = NULL, panel.last = NULL,
                                  legend = list(x = "topleft"), ...) {
  ## Prepare data
  n <- nrow(x)
  lab <- labels(x)

  ## Graphical parameters
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  if (length(lwd) == 1) lwd <- rep(lwd, length.out = n)

  lty <- list(...)$lty %||% graphics::par("lty")
  if (length(lty) == 1) lty <- rep(lty, length.out = n)
  if (!isFALSE(symbol)) lty <- khroma::palette_line(symbol)(lab)

  col <- list(...)$col %||% graphics::par("col")
  if (length(col) == 1) col <- rep(col, length.out = n)
  if (!isFALSE(color)) col <- khroma::palette_color_discrete(color)(lab)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(x@size)
  ylim <- range(x, na.rm = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  for (i in seq_len(n)) {
    graphics::lines(x = x@size, y = x[i, ], col = col[i],
                    lwd = lwd[i], lty = lty[i])
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct axis
  if (axes) {
    graphics::axis(side = 1, las = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- xlab %||% tr_("Sample size")
    ylab <- ylab %||% tr_("Expected species index")
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  ## Legend
  if (is.list(legend) && length(legend) > 0) {
    args <- list(legend = lab, col = col, lty = lty, lwd = lwd, bty = "n")
    args <- utils::modifyList(args, legend)
    do.call(graphics::legend, args = args)
  }

  invisible(x)
}

#' @export
#' @rdname plot_rarefaction
#' @aliases plot,RarefactionIndex,missing-method
setMethod("plot", c(x = "RarefactionIndex", y = "missing"), plot.RarefactionIndex)

# Index ========================================================================
#' @export
#' @rdname index_baxter
#' @aliases index_baxter,numeric-method
setMethod(
  f = "index_baxter",
  signature = signature(x = "numeric"),
  definition = function(x, sample, ...) {
    x <- x[x > 0]
    N <- sum(x)

    E <- suppressWarnings(exp(ramanujan(N - x) + ramanujan(N - sample) -
                                ramanujan(N - x - sample) - ramanujan(N)))
    sum(1 - E, na.rm = FALSE)
  }
)

#' @export
#' @rdname index_hurlbert
#' @aliases index_hurlbert,numeric-method
setMethod(
  f = "index_hurlbert",
  signature = signature(x = "numeric"),
  definition = function(x, sample, ...) {
    x <- x[x > 0]
    N <- sum(x)

    E <- vapply(
      X = x,
      FUN = function(x, N, sample) {
        if (N - x > sample) {
          combination(N - x, sample) / combination(N, sample)
        } else {
          0
        }
      },
      FUN.VALUE = double(1),
      N, sample
    )
    sum(1 - E, na.rm = FALSE)
  }
)
