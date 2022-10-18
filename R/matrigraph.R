# MATRIGRAPH
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname matrigraph
#' @aliases pvi,matrix-method
setMethod(
  f = "pvi",
  signature = signature(object = "matrix"),
  definition = function(object) {
    # Independance
    values <- expected(object)

    # Threshold
    threshold <- object / values

    dimnames(threshold) <- dimnames(object)
    threshold
  }
)

#' @export
#' @rdname matrigraph
#' @aliases pvi,data.frame-method
setMethod(
  f = "pvi",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

#' @export
#' @rdname matrigraph
#' @aliases matrigraph,matrix-method
setMethod(
  f = "matrigraph",
  signature = signature(object = "matrix"),
  definition = function(object, reverse = FALSE) {
    ## PVI
    pvi_data <- pvi(object)
    pvi_long <- arkhe::to_long(pvi_data, factor = FALSE)

    pvi_plus <- pvi_long[pvi_long$value > 1, ]
    pvi_plus$value <- pvi_plus$value - 1
    pvi_plus$value[pvi_plus$value > 1] <- 1

    if (reverse) {
      pvi_minus <- pvi_long[pvi_long$value < 1, ]
      pvi_minus$value <- 1 - pvi_minus$value
    } else {
      pvi_minus <- pvi_long
      pvi_minus$value[pvi_minus$value > 1] <- 1
    }

    bkg <- matrix(data = 1, nrow = nrow(object), ncol = ncol(object))
    bkg_long <- arkhe::to_long(bkg)

    col_bkg <- if (reverse) "darkgrey" else "white"
    col_minus <- if (reverse) "white" else "darkgrey"

    ggplot2::ggplot() +
      ggplot2::aes(x = .data$column, y = .data$row,
                   width = .data$value, height = .data$value) +
      ggplot2::geom_tile(data = bkg_long, fill = col_bkg) +
      ggplot2::geom_tile(data = pvi_minus, fill = col_minus) +
      ggplot2::geom_tile(data = pvi_plus, fill = "black") +
      ggplot2::coord_fixed() +
      scale_x_matrix(object, name = "Type") +
      scale_y_matrix(object, name = "Case") +
      theme_tabula()
  }
)

#' @export
#' @rdname matrigraph
#' @aliases matrigraph,data.frame-method
setMethod(
  f = "matrigraph",
  signature = signature(object = "data.frame"),
  definition = function(object, reverse = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, reverse = reverse)
  }
)
