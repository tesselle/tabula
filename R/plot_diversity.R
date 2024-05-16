# PLOT DIVERSITY
#' @include AllGenerics.R
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

# SHE ==========================================================================
#' @export
#' @rdname she
#' @aliases she,matrix-method
setMethod(
  f = "she",
  signature = c(object = "matrix"),
  definition = function(object, unbiased = FALSE,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "right"), ...) {
    ## Prepare data
    data <- .she(object, unbiased = unbiased)

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- range(data[, 4])
    ylim <- range(data[, -4])
    graphics::plot.window(xlim = xlim, ylim = ylim)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    col <- c("black", "red", "blue")
    for (i in c(1, 2, 3)) {
      graphics::lines(x = data[, 4], y = data[, i], col = col[i], lty = i)
      graphics::points(x = data[, 4], y = data[, i], col = col[i], pch = 16)
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
                      ylab = "Diversity", ...)
    }

    ## Legend
    if (is.list(legend) && length(legend) > 0) {
      args <- list(legend = c("ln(S)", "H", "ln(E)"),
                   col = col, pch = 16,
                   lty = c(1, 2, 3), bty = "n")
      args <- utils::modifyList(args, legend)
      do.call(graphics::legend, args = args)
    }

    invisible(object)
  }
)

#' @export
#' @rdname she
#' @aliases she,data.frame-method
setMethod(
  f = "she",
  signature = c(object = "data.frame"),
  definition = function(object, unbiased = FALSE,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "right"), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, unbiased = unbiased,
                         main = main, sub = sub, ann = ann,
                         axes = axes, frame.plot = frame.plot,
                         panel.first = panel.first, panel.last = panel.last,
                         legend = legend, ...)
  }
)

.she <- function(object, unbiased = FALSE, ...) {

  object <- data.matrix(object)
  n <- nrow(object)
  m <- ncol(object)

  SHE <- matrix(data = 0, nrow = n, ncol = 4)
  y <- numeric(m)

  for (i in seq_len(n)) {
    x <- object[i, ] + y
    n <- sum(x)

    ## Log species abundance
    S <- log(sum(x > 0)) # Observed species

    ## Shannon index
    H <- index_shannon(x, evenness = FALSE, unbiased = unbiased)

    ## log evenness
    E <- H - S

    SHE[i, ] <- c(S, H, E, n)
    y <- x
  }

  SHE
}
