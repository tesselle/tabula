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
                                xlab = NULL, ylab = NULL,
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
      HeterogeneityIndex = tr_("Heterogeneity"),
      EvennessIndex = tr_("Evenness"),
      RichnessIndex = tr_("Richness"),
      tr_("Diversity")
    )
    xlab <- xlab %||% tr_("Sample size")
    ylab <- ylab %||% sprintf("%s (%s)", y_lab, x@method)
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname plot.DiversityIndex
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
                        xlab = NULL, ylab = NULL,
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
      xlab <- xlab %||% tr_("Sample size")
      ylab <- ylab %||% tr_("Diversity")
      graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
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
                        xlab = NULL, ylab = NULL,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "right"), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, unbiased = unbiased,
                         xlab = xlab, ylab = ylab,
                         main = main, sub = sub, ann = ann,
                         axes = axes, frame.plot = frame.plot,
                         panel.first = panel.first, panel.last = panel.last,
                         legend = legend, ...)
  }
)

#' @param object A matrix.
#' @noRd
.she <- function(object, unbiased = FALSE, ...) {

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

# Profile ======================================================================
#' @export
#' @rdname profiles
#' @aliases profiles,matrix-method
setMethod(
  f = "profiles",
  signature = c(object = "matrix"),
  definition = function(object, alpha = seq(from = 0, to = 4, by = 0.04),
                        color = NULL, symbol = FALSE,
                        xlab = NULL, ylab = NULL,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topright"), ...) {
    ## Prepare data
    alpha <- alpha[alpha != 1]
    data <- .profiles(object, alpha = alpha)
    lab <- rownames(object)
    n <- nrow(object)

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
    xlim <- range(alpha)
    ylim <- range(data, finite = TRUE)
    graphics::plot.window(xlim = xlim, ylim = ylim)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    for (i in seq_len(n)) {
      graphics::lines(x = alpha, y = data[, i], col = col[i],
                      lty = lty[i], lwd = lwd[i])
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
      xlab <- xlab %||% "alpha"
      ylab <- ylab %||% tr_("Diversity")
      graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }

    ## Legend
    if (is.list(legend) && length(legend) > 0) {
      args <- list(legend = lab, col = col, lty = lty, lwd = lwd, bty = "n")
      args <- utils::modifyList(args, legend)
      do.call(graphics::legend, args = args)
    }

    invisible(object)
  }
)

#' @export
#' @rdname profiles
#' @aliases profiles,data.frame-method
setMethod(
  f = "profiles",
  signature = c(object = "data.frame"),
  definition = function(object, alpha = seq(from = 0, to = 4, by = 0.04),
                        color = NULL, symbol = FALSE,
                        xlab = NULL, ylab = NULL,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topright"), ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, alpha = seq(from = 0, to = 4, by = 0.04),
                         color = color, symbol = symbol,
                         xlab = xlab, ylab = ylab,
                         main = main, sub = sub, ann = ann,
                         axes = axes, frame.plot = frame.plot,
                         panel.first = panel.first, panel.last = panel.last,
                         legend = legend, ...)
  }
)

#' @param object A matrix.
#' @noRd
.profiles <- function(object, alpha = seq(from = 0, to = 4, by = 0.04), ...) {

  n <- nrow(object)
  m <- length(alpha)

  prof <- matrix(data = 0, nrow = m, ncol = n)

  index_renyi <- function(z, na.rm = FALSE) {
    z <- z[z > 0] # Remove unobserved species
    if (na.rm) z <- stats::na.omit(z) # Remove NAs

    function(x) {
      p <- z / sum(z)
      exp(log(sum(p^x)) / (1 - x))
    }
  }

  for (i in seq_len(n)) {
    x <- object[i, ]
    f <- index_renyi(x)
    prof[, i] <- vapply(X = alpha, FUN = f, FUN.VALUE = numeric(1), ...)
  }

  prof
}
