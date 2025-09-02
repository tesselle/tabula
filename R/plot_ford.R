# PLOT FORD
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_ford
#' @aliases plot_ford,matrix-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "matrix"),
  definition = function(object, weights = FALSE, EPPM = FALSE,
                        fill = "darkgrey", border = NA,
                        axes = TRUE, ...) {
    ## Prepare data
    n <- nrow(object)
    m <- ncol(object)
    seq_row <- rev(seq_len(n))
    seq_col <- seq_len(m)
    lab_row <- rownames(object) %||% seq_row
    lab_col <- colnames(object) %||% seq_col

    padding_x <- 0.05
    padding_y <- 0.5 - 0.01
    data <- prepare_ford(object, padding = padding_x)

    ## Graphical parameters
    cex.axis <- graphics::par("cex.axis")
    col.axis <- graphics::par("col.axis")
    font.axis <- graphics::par("font.axis")

    ## Save and restore
    d <- inch2line("M", cex = cex.axis)
    mfrow <- graphics::par("mfrow")
    mar <- graphics::par("mar")
    mar[1] <- 3
    mar[2] <- inch2line(lab_row, cex = cex.axis) + 0.1
    mar[3] <- inch2line(lab_col, cex = cex.axis) + 0.1
    mar[4] <- 0

    old_par <- graphics::par(mfrow = mfrow, mar = mar)
    on.exit(graphics::par(old_par))

    if (weights) {
      graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(m - 1, 1))
    }

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- range(data$x - data$value, data$x + data$value, na.rm = TRUE)
    ylim <- range(data$y) + c(-1, 1) * padding_y
    graphics::plot.window(xlim = xlim, ylim = ylim)

    ## Plot
    graphics::rect(
      xleft = data$x - data$value,
      ybottom = data$y - padding_y,
      xright = data$x + data$value,
      ytop = data$y + padding_y,
      col = fill,
      border = border
    )
    if (EPPM) {
      graphics::rect(
        xleft = data$x - data$eppm,
        ybottom = data$y - padding_y,
        xright = data$x + data$eppm,
        ytop = data$y + padding_y,
        col = "black",
        border = NA
      )
    }

    ## Construct axis
    if (axes) {
      graphics::axis(side = 2, at = seq_row, labels = lab_row, las = 2,
                     lty = 0, cex.axis = cex.axis, col.axis = col.axis,
                     font.axis = font.axis)
      graphics::axis(side = 3, at = unique(data$x), labels = lab_col, las = 2,
                     lty = 0, cex.axis = cex.axis, col.axis = col.axis,
                     font.axis = font.axis)

      x_axis <- data$x[which.max(data$value)]
      graphics::axis(side = 1, at = c(x_axis - 0.2, x_axis + 0.2), labels = FALSE)
      graphics::axis(side = 1, at = x_axis, labels = label_percent(0.2),
                     tick = FALSE)
    }

    if (weights) {
      ## Graphical parameters
      mar[2] <- 0.1
      mar[4] <- 0.1
      graphics::par(mar = mar)

      ## Open new window
      graphics::plot.new()

      ## Set plotting coordinates
      total <- rowSums(object)
      graphics::plot.window(xlim = c(0, max(total) * 1.05), ylim = ylim,
                            xaxs = "i")

      ## Plot
      y <- length(total) - seq_along(total) + 1
      graphics::rect(
        xleft = 0,
        ybottom = y - padding_y,
        xright = total,
        ytop = y + padding_y,
        col = fill,
        border = border
      )

      ## Construct axis
      if (axes) {
        graphics::segments(x0 = 0, y0 = 0, x1 = 0, y = n + 0.5,
                           col = col.axis, lwd = 1)
        graphics::axis(side = 1)
      }
    }

    invisible(object)
  }
)

#' @export
#' @rdname plot_ford
#' @aliases plot_ford,data.frame-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "data.frame"),
  definition = function(object, weights = FALSE, EPPM = FALSE,
                        fill = "darkgrey", border = NA,
                        axes = TRUE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, weights = weights, EPPM = EPPM, fill = fill,
                         border = border, axes = axes, ...)
  }
)

#' Prepare data for Ford plot
#' @return A data.frame.
#' @keywords internal
#' @noRd
prepare_ford <- function(x, padding = 0.05) {
  ## EPPM
  EPPM <- eppm(x) / 100

  ## Relative frequencies
  freq <- x / rowSums(x)

  ## Prevent division by zero
  freq[is.nan(freq)] <- 0

  ## Adaptive spacing between columns
  col_max <- apply(X = freq, MARGIN = 2, FUN = max, na.rm = TRUE)
  roll_max <- roll_sum(col_max, n = 2) + padding * max(freq)
  cum_max <- c(0, cumsum(roll_max))

  ## Build a long table
  row <- row(x, as.factor = TRUE)
  col <- col(x, as.factor = TRUE)
  data <- data.frame(
    row = as.vector(row),
    column = as.vector(col),
    value = as.vector(freq),
    eppm = as.vector(EPPM)
  )

  m <- nrow(freq)
  data$x <- rep(cum_max, each = m) + 1
  data$y <- as.vector(m + 1 - as.numeric(row)) # Reverse levels order

  return(data)
}
