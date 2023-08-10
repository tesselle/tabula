# MATRIX PLOT

# Plot =========================================================================
#' @param panel A [`function`] in the form `function(x, y, z, color, ...)`
#'  which gives the action to be carried out in each panel of the display.
#' @param ... Further arguments to be passed to `panel`.
#' @keywords internal
#' @noRd
.plot_matrix <- function(object, panel, col = graphics::par("fg"),
                         midpoint = NULL, diag = TRUE, upper = TRUE, lower = TRUE,
                         freq = FALSE, scale = TRUE, drop_zero = TRUE,
                         axes = TRUE, legend = TRUE,
                         asp = 1, ...) {
  ## Validation
  if (is_incidence(object)) legend <- FALSE

  ## Prepare data
  n <- nrow(object)
  m <- ncol(object)
  seq_row <- rev(seq_len(n))
  seq_col <- seq_len(m)
  lab_row <- rownames(object) %||% seq_row
  lab_col <- colnames(object) %||% seq_col

  data <- prepare(object, diag = diag, upper = upper, lower = lower,
                  freq = freq, scale = scale, drop_zero = drop_zero,
                  palette = col, midpoint = midpoint)

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
  xmax <- m + isTRUE(legend) + 0.5
  ymax <- n + 1.5

  d <- graphics::strwidth("M", units = "user", cex = cex.axis) * 2
  left <- max(graphics::strwidth(lab_row, units = "user", cex = cex.axis)) + d
  right <- graphics::strwidth("999%", units = "user", cex = cex.axis) * isTRUE(legend)
  width <- left + right

  left <- left * (width + xmax)
  right <- right * (width + xmax)
  top <- max(graphics::strwidth(lab_col, units = "user", cex = cex.axis)) + d
  top <- top * (top + ymax)

  asp_ratio <- xmax / ymax
  if (ymax > xmax) left <- left / asp_ratio
  else if (ymax < xmax) top <- top * asp_ratio

  xlim <- c(-left, right + xmax)
  ylim <- c(0, top + ymax)
  graphics::plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", asp = asp)

  ## Plot
  panel(x = data$x, y = data$y, z = data$scaled, color = data$color, ...)

  ## Construct axis
  if (axes) {
    graphics::text(x = 0, y = seq_row, labels = lab_row, adj = c(1, 0.5),
                   cex = cex.axis, col = col.axis, font = font.axis)
    graphics::text(x = seq_col, y = n + 1, labels = lab_col, adj = c(0, 0.5),
                   cex = cex.axis, col = col.axis, font = font.axis, srt = 90)
  }

  ## Legend
  if (legend) {
    legend <- attr(data, "legend")
    legend_image <- grDevices::as.raster(legend$colors)
    legend_y <- scale_range(legend$at) * n + 0.5

    graphics::rasterImage(legend_image, xleft = m + 1, ybottom = max(legend_y),
                          xright = m + 1.5, ytop = min(legend_y))
    graphics::segments(x0 = m + 1, y0 = legend_y, x1 = m + 1.5, y1 = legend_y,
                       col = "white")
    graphics::polygon(x = c(m, m + 0.5, m + 0.5, m) + 1,
                      y = c(0.5, 0.5, max(legend_y), max(legend_y)),
                      col = NA, border = "black")
    graphics::text(x = xmax, y = legend_y, labels = legend$labels, pos = 4,
                   cex = cex.axis, col = col.axis, font = font.axis)
  }
}

# Panels =======================================================================
panel_bertin <- function(x, y, z, color, ..., space = 0.05) {
  y_bottom <- y - 0.5
  y_top <- y - 0.5 + z - space * 2
  y_top[y_top < y_bottom] <- y_bottom[y_top < y_bottom]
  graphics::rect(
    xleft = x - 0.5 + space,
    ybottom = y_bottom,
    xright = x + 0.5 - space,
    ytop = y_top,
    col = color,
    border = "black"
  )
}
panel_matrigraph <- function(x, y, z, ..., reverse = FALSE) {
  pvi <- data.frame(x = x, y = y, z = z)
  pvi_plus <- pvi[pvi$z > 1, ]
  pvi_plus$z <- pvi_plus$z - 1
  pvi_plus$z[pvi_plus$z > 1] <- 1

  if (reverse) {
    pvi_minus <- pvi[pvi$z < 1, ]
    pvi_minus$z <- 1 - pvi_minus$z
  } else {
    pvi_minus <- pvi
    pvi_minus$z[pvi_minus$z > 1] <- 1
  }

  pvi_minus$z <- pvi_minus$z * 0.5
  pvi_plus$z <- pvi_plus$z * 0.5

  col_bkg <- if (reverse) "darkgrey" else "white"
  col_minus <- if (reverse) "white" else "darkgrey"

  graphics::rect(xleft = pvi$x - 0.5, ybottom = pvi$y - 0.5,
                 xright = pvi$x + 0.5, ytop = pvi$y + 0.5,
                 col = col_bkg, border = NA)
  graphics::rect(xleft = pvi_minus$x - pvi_minus$z,
                 ybottom = pvi_minus$y - pvi_minus$z,
                 xright = pvi_minus$x + pvi_minus$z,
                 ytop = pvi_minus$y + pvi_minus$z,
                 col = col_minus, border = NA)
  graphics::rect(xleft = pvi_plus$x - pvi_plus$z,
                 ybottom = pvi_plus$y - pvi_plus$z,
                 xright = pvi_plus$x + pvi_plus$z,
                 ytop = pvi_plus$y + pvi_plus$z,
                 col = "black", border = NA)
}
panel_tiles <- function(x, y, color, ...) {
  width <- 0.5
  graphics::rect(
    xleft = x - width,
    ybottom = y - width,
    xright = x + width,
    ytop = y + width,
    col = color,
    border = NA
  )
}
panel_spot <- function(x, y, z, color, type, ...) {
  radius <- abs(z * 0.45)
  for (i in seq_along(x)) {
    plot_circle(x = x[i], y = y[i], radius = radius[i], col = color[i],
                border = color[i])
    if (type == "ring") {
      plot_circle(x = x[i], y = y[i], radius = 0.45, col = NA, border = "black")
    }
  }
}

# Prepare ======================================================================
#' Prepare Data for Matrix Plot
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] of count data.
#' @param diag A [`logical`] scalar indicating whether the diagonal of the
#'  matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param upper A [`logical`] scalar indicating whether the upper triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param lower A [`logical`] scalar indicating whether the lower triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param freq A [`logical`] scalar indicating whether relative frequencies
#'  should be used instead of absolute frequencies.
#' @param scale A [`logical`] scalar indicating whether data should be rescaled
#'  to \eqn{[0,1]}.
#' @param drop_zero A [`logical`] scalar indicating whether zeros must be
#'  removed.
#' @param palette A vector of colors.
#' @param midpoint A [`numeric`] value specifying the data midpoint.
#' @param ... Currently not used.
#' @return
#'  A long [`data.frame`].
#'  \describe{
#'   \item{`row`}{}
#'   \item{`column`}{}
#'   \item{`x`,`y`}{Tile center coordinates.}
#'   \item{`z`}{Raw values.}
#'   \item{`value`}{}
#'   \item{`scaled`}{}
#'   \item{`color`}{}
#'  }
#' @keywords internal
#' @noRd
prepare <- function(object, diag = TRUE, upper = TRUE, lower = TRUE,
                    freq = FALSE, scale = !freq, drop_zero = FALSE,
                    palette = khroma::colour("YlOrBr")(12),
                    midpoint = NULL, ...) {
  ## Validation
  if (!arkhe::is_symmetric(object)) {
    diag <- TRUE
    upper <- TRUE
    lower <- TRUE
  }

  ## Coerce to matrix
  object <- as.matrix(object)

  ## Rescale
  val <- object
  if (freq) val <- object / rowSums(object, na.rm = TRUE) # Relative frequencies
  val <- sca <- as.vector(val)
  if (scale) sca <- val / max(abs(val), na.rm = TRUE) # Must be in [-1;1]

  min_val <- min(val, na.rm = TRUE)
  max_val <- max(val, na.rm = TRUE)

  ## Build a long table
  row <- row(object, as.factor = TRUE)
  col <- col(object, as.factor = TRUE)
  data <- data.frame(
    row = as.vector(row),
    column = as.vector(col),
    x = as.numeric(col),
    y = as.vector(nrow(object) - as.numeric(row) + 1), # Reverse y for plotting
    z = as.vector(object),
    value = val,
    scaled = sca
  )

  ## Map colors
  if (length(palette) != length(val)) {
    midpoint <- if (is.null(midpoint) & min_val < 0 & max_val > 0) 0 else midpoint
    data$color <- color_ramp(val, palette = palette, from = c(min_val, max_val),
                             midpoint = midpoint)
  } else {
    data$color <- palette
  }

  ## Legend
  legend_brk <- pretty(val, n = 3)
  legend_lab <- legend_brk[legend_brk > min_val & legend_brk < max_val]
  legend_lab <- c(min_val, legend_lab, max_val)
  legend_pos <- legend_lab / max(abs(val), na.rm = TRUE)
  legend_col <- color_ramp(legend_lab, palette = palette,
                           from = c(min_val, max_val), midpoint = midpoint)

  ## Clean data
  if (!upper) {
    data <- data[!upper.tri(object), ]
  }
  if (!lower) {
    data <- data[!lower.tri(object), ]
  }
  if (!diag) {
    data <- data[data$row != data$column, ]
  }
  if (drop_zero) {
    data <- data[data$value != 0, ]
  }

  legend_lab[!is.element(legend_lab, legend_brk)] <- NA
  attr(data, "legend") <- list(
    labels = if (freq) scale_pc(legend_lab) else legend_lab,
    at = legend_pos,
    colors = legend_col
  )
  data
}
