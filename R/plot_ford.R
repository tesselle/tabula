# PLOT FORD
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_ford
#' @aliases plot_ford,matrix-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "matrix"),
  definition = function(object, weights = FALSE, EPPM = FALSE, col = "darkgrey",
                        axes = TRUE, ...) {
    ## Prepare data
    n <- nrow(object)
    m <- ncol(object)
    seq_row <- rev(seq_len(n))
    seq_col <- seq_len(m)
    lab_row <- rownames(object) %||% seq_row
    lab_col <- colnames(object) %||% seq_col

    data <- prepare_ford(object)

    ## Graphical parameters
    cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
    col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
    font.axis <- list(...)$font.axis %||% graphics::par("font.axis")

    ## Save and restore
    mar <- rep(0.1, 4)
    old_par <- graphics::par(mar = mar)
    on.exit(graphics::par(old_par))

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xmax <- max(data$x + data$value) + 0.1
    ymax <- n + 1.5

    d <- graphics::strwidth("M", units = "user", cex = cex.axis) * 2
    left <- max(graphics::strwidth(lab_row, units = "user", cex = cex.axis)) + d
    left <- left * (left + xmax)
    top <- max(graphics::strwidth(lab_col, units = "user", cex = cex.axis)) + d
    top <- top * (top + ymax)

    xlim <- c(-left, xmax)
    ylim <- c(0, top + ymax)
    graphics::plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i")

    ## Plot
    graphics::rect(
      xleft = data$x - data$value,
      ybottom = data$y - 0.5,
      xright = data$x + data$value,
      ytop = data$y + 0.5,
      col = col,
      border = NA
    )
    if (EPPM) {
      graphics::rect(
        xleft = data$x - data$eppm,
        ybottom = data$y - 0.5,
        xright = data$x + data$eppm,
        ytop = data$y + 0.5,
        col = "black",
        border = NA
      )
    }

    ## Construct axis
    if (axes) {
      graphics::text(x = 0, y = seq_row, labels = lab_row, adj = c(1, 0.5),
                     cex = cex.axis, col = col.axis, font = font.axis)
      graphics::text(x = unique(data$x), y = n + 1, labels = lab_col, adj = c(0, 0.5),
                     cex = cex.axis, col = col.axis, font = font.axis, srt = 90)
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
  definition = function(object, weights = FALSE, EPPM = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, weights = weights, EPPM = EPPM)
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

  ## Adaptive spacing between columns
  col_max <- apply(X = freq, MARGIN = 2, FUN = max, na.rm = TRUE)
  roll_max <- roll_sum(col_max, n = 2) + padding * max(freq)
  cum_max <- c(0, cumsum(roll_max))

  ## Build a long table for ggplot2 (preserve original ordering)
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
