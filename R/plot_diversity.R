# PLOT DIVERSITY
#' @include AllClasses.R AllGenerics.R
NULL

# DiversityIndex ===============================================================
#' @export
#' @method plot DiversityIndex
plot.DiversityIndex <- function(x, log = "x",
                                col.mean = "#DDAA33", col.interval = "#004488",
                                lty.mean = "solid", lty.interval = "dashed",
                                lwd.mean = 1, lwd.interval = 1,
                                main = NULL, sub = NULL,
                                ann = graphics::par("ann"),
                                axes = TRUE, frame.plot = axes,
                                panel.first = NULL, panel.last = NULL, ...) {
  ## Prepare data
  count <- as.data.frame(x)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(count$size)
  ylim <- range(count$index)
  if (length(x@simulation) != 0) {
    xlim <- range(x@simulation[, "size"])
    ylim <- range(x@simulation[, c("lower", "upper")])
  }
  graphics::plot.window(xlim = xlim, ylim = ylim, log = log)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  graphics::points(x = count$size, y = count$index, ...)

  ## Simulated assemblages
  if (length(x@simulation) != 0) {
    refined <- x@simulation
    graphics::lines(x = refined[, "size"], y = refined[, "mean"],
                    col = col.mean, lty = lty.mean, lwd = lwd.mean)
    graphics::lines(x = refined[, "size"], y = refined[, "lower"],
                    col = col.interval, lty = lty.interval, lwd = lwd.interval)
    graphics::lines(x = refined[, "size"], y = refined[, "upper"],
                    col = col.interval, lty = lty.interval, lwd = lwd.interval)
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
    y_lab <- switch (
      class(x),
      HeterogeneityIndex = "Heterogeneity",
      EvennessIndex = "Evenness",
      RichnessIndex = "Richness",
      "Diversity"
    )
    graphics::title(main = main, sub = sub, xlab = "Sample size",
                    ylab = sprintf("%s (%s)", y_lab, x@method))
  }

  invisible(x)
}

#' @export
#' @rdname plot_diversity
#' @aliases plot,DiversityIndex,missing-method
setMethod("plot", c(x = "DiversityIndex", y = "missing"), plot.DiversityIndex)

# RarefactionIndex =============================================================
#' @export
#' @method plot RarefactionIndex
plot.RarefactionIndex <- function(x,
                                  main = NULL, sub = NULL,
                                  ann = graphics::par("ann"),
                                  axes = TRUE, frame.plot = axes,
                                  panel.first = NULL, panel.last = NULL,
                                  legend = list(x = "topleft"), ...) {
  ## Prepare data
  n <- nrow(x)

  ## Graphical parameters
  col <- list(...)$col %||% grDevices::hcl.colors(n, "viridis")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  lty <- list(...)$lty %||% graphics::par("lty")
  if (length(lwd) < n) lwd <- rep(lwd, length.out = n)
  if (length(lty) < n) lty <- rep(lty, length.out = n)
  if (length(col) < n) col <- rep(col, length.out = n)

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
    graphics::title(main = main, sub = sub, xlab = "Sample size",
                    ylab = "Expected species index")
  }

  ## Legend
  if (is.list(legend) && length(legend) > 0) {
    args <- list(legend = rownames(x), col = col, lty = lty, lwd = lwd, bty = "n")
    args <- utils::modifyList(args, legend)
    do.call(graphics::legend, args = args)
  }

  invisible(x)
}

#' @export
#' @rdname plot_rarefaction
#' @aliases plot,RarefactionIndex,missing-method
setMethod("plot", c(x = "RarefactionIndex", y = "missing"), plot.RarefactionIndex)
