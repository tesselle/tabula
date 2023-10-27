# PLOT DICE-LERASS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_diceleraas
#' @aliases plot_diceleraas,matrix-method
setMethod(
  f = "plot_diceleraas",
  signature = signature(object = "matrix"),
  definition = function(object,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = FALSE,
                        panel.first = NULL, panel.last = NULL, ...) {
    ## Prepare data
    object <- t(object)
    object[object == 0] <- NA
    n <- nrow(object)
    id <- seq_len(n)
    lab <- rownames(object) %||% id

    ## Mean
    moy <- rowMeans(object, na.rm = TRUE)
    ## Standard deviation
    ec <- apply(X = object, MARGIN = 1, FUN = stats::sd, na.rm = TRUE)
    ## Standard error
    se <- ec / sqrt(rowSums(!is.na(object)))
    ## Range
    ran <- t(apply(X = object, MARGIN = 1, FUN = range, na.rm = TRUE))
    colnames(ran) <- c("min", "max")

    data <- data.frame(y = rev(id), mean = moy, sd = ec, se = se * 2, ran)

    ## Graphical parameters
    cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
    col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
    font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
    lwd <- list(...)$lwd %||% graphics::par("lwd")
    lty <- list(...)$lty %||% graphics::par("lty")
    col <- list(...)$col %||% c("black")
    if (length(lwd) < n) lwd <- rep(lwd, length.out = n)
    if (length(lty) < n) lty <- rep(lty, length.out = n)
    if (length(col) < n) col <- rep(col, length.out = n)

    ## Save and restore
    mar <- graphics::par("mar")
    mar[2] <- arkhe::inch2line(lab, cex = cex.axis) + 0.5
    old_par <- graphics::par(mar = mar)
    on.exit(graphics::par(old_par))

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- range(data$min, data$max, data$mean + data$sd,
                  data$mean - data$sd, na.rm = TRUE)
    ylim <- c(1, n + 1)
    graphics::plot.window(xlim = xlim, ylim = ylim)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    for (i in id) {
      tmp <- data[i, ]
      graphics::polygon(
        x = tmp$sd * c(-1, -1, 1, 1) + tmp$mean,
        y = tmp$y + (1 / 3) * c(0, 1, 1, 0),
        col = "white", border = NA
      )
      graphics::lines(
        x = tmp$sd * c(-1, -1, 1, 1) + tmp$mean,
        y = tmp$y + (1 / 3) * c(0, 1, 1, 0),
        col = col[i], lty = lty[i]
      )
      graphics::polygon(
        x = tmp$se * c(-1, -1, 1, 1) + tmp$mean,
        y = tmp$y + (1 / 3) * c(0, 1, 1, 0),
        col = col[i], border = NA
      )
      graphics::segments(
        x0 = tmp$mean, y0 = tmp$y,
        x1 = tmp$mean, y1 = tmp$y + 0.5,
        col = col[i], lty = lty[i]
      )
      graphics::segments(
        x0 = tmp$min, y0 = tmp$y,
        x1 = tmp$max, y1 = tmp$y,
        col = col[i], lty = lty[i]
      )
    }

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct axis
    if (axes) {
      graphics::axis(side = 1, las = 1)
      graphics::mtext(rev(lab), side = 2, at = id, las = 1, padj = 0, line = 0,
                      cex = cex.axis, col.axis = col.axis, font = font.axis)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      graphics::title(main = main, sub = sub, xlab = "Count", ylab = NULL, ...)
    }

    invisible(object)
  }
)

#' @export
#' @rdname plot_diceleraas
#' @aliases plot_diceleraas,data.frame-method
setMethod(
  f = "plot_diceleraas",
  signature = signature(object = "data.frame"),
  definition = function(object,
                        main = NULL, sub = NULL,
                        ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = FALSE,
                        panel.first = NULL, panel.last = NULL, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object,
                         main = main, sub = sub, ann = ann,
                         axes = axes, frame.plot = frame.plot,
                         panel.first = panel.first, panel.last = panel.last, ...)
  }
)
