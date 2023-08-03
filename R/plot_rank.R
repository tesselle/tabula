# PLOT RANK
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_rank
#' @aliases plot_rank,matrix-method
setMethod(
  f = "plot_rank",
  signature = c(object = "matrix"),
  definition = function(object, log = NULL,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topright"), ...) {
    ## Prepare data
    n <- nrow(object)
    object <- object / rowSums(object)
    rk <- t(apply(X = object, MARGIN = 1, FUN = function(x) rank(-x)))

    ## Graphical parameters
    pch <- list(...)$pch %||% 16
    lwd <- list(...)$lwd %||% graphics::par("lwd")
    lty <- list(...)$lty %||% graphics::par("lty")
    cex <- list(...)$cex %||% graphics::par("cex")
    col <- list(...)$col %||% grDevices::hcl.colors(n, "viridis")
    if (length(pch) < n) pch <- rep(pch, length.out = n)
    if (length(lwd) < n) lwd <- rep(lwd, length.out = n)
    if (length(lty) < n) lty <- rep(lty, length.out = n)
    if (length(cex) < n) cex <- rep(cex, length.out = n)
    if (length(col) < n) col <- rep(col, length.out = n)
    if (is.null(log)) log <- ""

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- range(rk)
    ylim <- range(object)
    graphics::plot.window(xlim = xlim, ylim = ylim, log = log)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    for (i in seq_len(n)) {
      z <- order(rk[i, ])
      graphics::lines(x = rk[i, z], y = object[i, z],
                      col = col[i], lty = lty[i], lwd = lwd[i])
      graphics::points(x = rk[i, z], y = object[i, z],
                       col = col[i], pch = pch[i], cex = cex[i])
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
      graphics::title(main = main, sub = sub, xlab = "Rank",
                      ylab = "Frequency", ...)
    }

    ## Legend
    if (is.list(legend) && length(legend) > 0) {
      args <- list(legend = rownames(object), col = col, pch = pch,
                   lty = lty, lwd = lwd, bty = "n")
      args <- utils::modifyList(args, legend)
      do.call(graphics::legend, args = args)
    }

    invisible(object)
  }
)

#' @export
#' @rdname plot_rank
#' @aliases plot_rank,data.frame-method
setMethod(
  f = "plot_rank",
  signature = signature(object = "data.frame"),
  definition = function(object, log = NULL,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topright"), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, log = log,
                         main = main, sub = sub, ann = ann, axes = axes,
                         frame.plot = frame.plot, panel.first = panel.first,
                         panel.last = panel.last, legend = legend, ...)
  }
)
