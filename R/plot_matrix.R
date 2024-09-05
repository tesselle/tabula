# MATRIX PLOT

# Plot =========================================================================
#' Matrix Plot
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table).
#' @param panel A [`function`] in the form `function(x, y, z, color, ...)`
#'  which gives the action to be carried out in each panel of the display.
#' @param diag A [`logical`] scalar indicating whether the diagonal of the
#'  matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param upper A [`logical`] scalar indicating whether the upper triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param lower A [`logical`] scalar indicating whether the lower triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param freq A [`logical`] scalar indicating whether conditional proportions
#'  given `margins` should be used (i.e. entries of `object`, divided by the
#'  appropriate marginal sums).
#' @param margin An [`integer`] vector giving the margins to split by:
#'  `1` indicates individuals/rows (the default), `2` indicates
#'  variables/columns. Only used if `freq` is `TRUE`.
#' @param scale A [`logical`] scalar indicating whether data should be rescaled
#'  to \eqn{[-1,1]}. Only used if `freq` if `FALSE`.
#' @param drop_zero A [`logical`] scalar: should zeros be discarded?
#' @param color A vector of colors or a [`function`] that when called with a
#'  single argument (an integer specifying the number of colors) returns a
#'  vector of colors.
#' @param midpoint A [`numeric`] value specifying the data midpoint.
#' @param axes A [`logical`] scalar: should axes be drawn on the plot? It will
#'  omit labels where they would abut or overlap previously drawn labels.
#' @param legend A [`logical`] scalar: should a legend be displayed?
#' @param asp A length-one [`numeric`] vector, giving the aspect ratio
#'  \eqn{y/x}.
#' @param ... Further arguments to be passed to `panel`.
#' @keywords internal
plot_matrix <- function(object, panel, diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, margin = 1, scale = TRUE, drop_zero = TRUE,
                        color = graphics::par("fg"), midpoint = NULL,
                        axes = TRUE, legend = TRUE, asp = 1, ...) {
  ## Validation
  if (is_incidence(object)) {
    object[] <- as.numeric(object)
    legend <- FALSE
  }

  ## Prepare data
  n <- nrow(object)
  m <- ncol(object)
  seq_row <- rev(seq_len(n))
  seq_col <- seq_len(m)
  lab_row <- rownames(object) %||% seq_row
  lab_col <- colnames(object) %||% seq_col

  data <- prepare(object, diag = diag, upper = upper, lower = lower,
                  freq = freq, margin = margin, scale = scale,
                  drop_zero = drop_zero,
                  palette = color, midpoint = midpoint)

  ## Graphical parameters
  cex.axis <- graphics::par("cex.axis")
  col.axis <- graphics::par("col.axis")
  font.axis <- graphics::par("font.axis")

  ## Save and restore
  d <- inch2line("M", cex = cex.axis)
  old_par <- graphics::par("mar", "plt")
  on.exit(graphics::par(old_par))

  mar_left <- inch2line(lab_row, cex = cex.axis)
  mar_top <- inch2line(lab_col, cex = cex.axis)
  mar_right <- if (legend) inch2line("999%", cex = cex.axis) else d
  graphics::par(mar = c(d, mar_left, mar_top, mar_right))

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Squish plotting area
  pin <- graphics::par("pin")
  plt <- graphics::par("plt")

  if (isTRUE(asp)) asp <- 1
  if (!isFALSE(asp) && !is.na(asp)) {
    aspect_ratio <- n / (m + legend)
    pin_y <- pin[1] * aspect_ratio * asp

    if (pin_y < pin[2]) {
      ## Squish vertically
      graphics::par(pin = c(pin[1], pin_y))
      graphics::par(plt = c(plt[1:2], graphics::par('plt')[3:4]))
    } else {
      ## Squish horizontally
      pin_x <- pin[2] / aspect_ratio / asp
      graphics::par(pin = c(pin_x, pin[2]))
      graphics::par(plt = c(graphics::par('plt')[1:2], plt[3:4]))
    }
  }

  ## Set plotting coordinates
  xlim <- c(0, m + legend) + 0.5
  ylim <- c(0, n) + 0.5
  graphics::plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", asp = asp)

  ## Plot
  panel(x = data$x, y = data$y, z = data$scaled, color = data$color, ...)

  ## Construct axis
  if (axes) {
    graphics::axis(side = 2, at = seq_row, labels = lab_row, las = 2,
                   lty = 0, cex.axis = cex.axis, col.axis = col.axis,
                   font.axis = font.axis)
    graphics::axis(side = 3, at = seq_col, labels = lab_col, las = 2,
                   lty = 0, cex.axis = cex.axis, col.axis = col.axis,
                   font.axis = font.axis)
  }

  ## Legend
  if (legend) {
    lgd <- attr(data, "legend")
    legend_gradient(x = m, y = n, labels = lgd$labels,
                    at = lgd$at, col = lgd$colors)
  }
}

legend_gradient <- function(x, y, labels, at, col) {
  legend_image <- grDevices::as.raster(col)
  legend_y <- (at - min(at)) * y / diff(range(at)) + 0.5

  graphics::rasterImage(legend_image, xleft = x + 1, ybottom = max(legend_y),
                        xright = x + 1.5, ytop = min(legend_y))
  graphics::segments(x0 = x + 1, y0 = legend_y, x1 = x + 1.5, y1 = legend_y,
                     col = "white")
  graphics::polygon(x = c(x, x + 0.5, x + 0.5, x) + 1,
                    y = c(0.5, 0.5, max(legend_y), max(legend_y)),
                    col = NA, border = "black")
  graphics::axis(side = 4, at = legend_y, labels = labels, las = 2)
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
  pvi$z <- pvi$z / 100
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
    circle(x = x[i], y = y[i], radius = radius[i],
                     col = color[i], border = color[i])
    if (type == "ring") {
      circle(x = x[i], y = y[i], radius = 0.45,
                       col = NA, border = "black")
    }
  }
}

# Prepare ======================================================================
#' Prepare Data for Matrix Plot
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table).
#' @param diag A [`logical`] scalar indicating whether the diagonal of the
#'  matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param upper A [`logical`] scalar indicating whether the upper triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param lower A [`logical`] scalar indicating whether the lower triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param freq A [`logical`] scalar indicating whether conditional proportions
#'  given `margins` should be used (i.e. entries of `object`, divided by the
#'  appropriate marginal sums).
#' @param margin An [`integer`] vector giving the margins to split by:
#'  `1` indicates individuals/rows (the default), `2` indicates
#'  variables/columns. Only used if `freq` is `TRUE`.
#' @param scale A [`logical`] scalar indicating whether data should be rescaled
#'  to \eqn{[-1,1]}. Only used if `freq` if `FALSE`.
#' @param drop_zero A [`logical`] scalar: should zeros be discarded?
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
                    freq = FALSE, margin = 1, scale = !freq, drop_zero = FALSE,
                    palette = grDevices::hcl.colors(12, "YlOrBr", rev = TRUE),
                    midpoint = NULL, ...) {
  ## Validation
  if (!arkhe::is_symmetric(object)) {
    diag <- TRUE
    upper <- TRUE
    lower <- TRUE
  }

  ## Coerce to matrix
  object <- as.matrix(object)

  ## Relative frequencies
  val <- if (freq) prop.table(object, margin = margin) else object

  ## Rescale to [-1;1]
  sca <- if (scale) val / max(abs(val), na.rm = TRUE) else val

  val <- as.vector(val)
  sca <- as.vector(sca)
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
  breaks <- pretty(val, n = 5)
  domain <- range(c(breaks, min_val, max_val))
  midpoint <- if (is.null(midpoint) & min_val < 0 & max_val > 0) 0 else midpoint
  pal <- khroma::palette_color_continuous(colors = palette, domain = domain, midpoint = midpoint)
  data$color <- if (length(palette) == length(val)) palette else pal(val)

  ## Clean data
  if (!upper) data <- data[!upper.tri(object), ]
  if (!lower) data <- data[!lower.tri(object), ]
  if (!diag) data <- data[data$row != data$column, ]
  if (drop_zero) data <- data[data$value != 0, ]

  ## Legend
  attr(data, "legend") <- list(
    labels = if (freq) label_percent(breaks) else breaks,
    at = breaks / max(abs(val), na.rm = TRUE),
    colors = pal(breaks)
  )
  data
}
