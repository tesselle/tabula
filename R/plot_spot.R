# PLOT SPOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,matrix-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "matrix"),
  definition = function(object, type = c("ring", "plain"), threshold = NULL,
                        diag = TRUE, upper = TRUE, lower = TRUE, freq = FALSE,
                        ...) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    ## Prepare data
    data <- prepare_heatmap(object, diag = diag, upper = upper, lower = lower,
                            freq = freq, threshold = threshold,
                            drop_zero = TRUE, ...)

    ## ggplot
    gg_ring <- NULL
    if (type == "ring") {
      gg_ring <- list(
        ggplot2::geom_point(
          mapping = ggplot2::aes(size = .data$max * 1.2),
          colour = "black",
          show.legend = FALSE
        ),
        ggplot2::geom_point(
          mapping = ggplot2::aes(size = .data$max),
          colour = "white",
          show.legend = FALSE
        )
      )
    }

    aes_point <- ggplot2::aes(size = .data$value, colour = .data$value)
    if (!is.null(threshold)) {
      aes_point <- ggplot2::aes(size = .data$value, colour = .data$threshold)
    }

    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y) +
      gg_ring +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::coord_fixed() +
      ggplot2::scale_size_area(name = "value") +
      scale_x_matrix(object, name = "Type") +
      scale_y_matrix(object, name = "Case") +
      theme_tabula()
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,data.frame-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "data.frame"),
  definition = function(object, type = c("ring", "plain"), threshold = NULL,
                        diag = TRUE, upper = TRUE, lower = TRUE,
                        freq = FALSE, ...) {
    object <- data.matrix(object)
    methods::callGeneric(object, type = type, threshold = threshold,
                         diag = diag, upper = upper, lower = lower,
                         freq = freq, ...)
  }
)

#' @export
#' @rdname plot_spot
#' @aliases plot_spot,dist-method
setMethod(
  f = "plot_spot",
  signature = signature(object = "dist"),
  definition = function(object, type = c("ring", "plain"), diag = FALSE,
                        upper = FALSE, lower = !upper, ...) {
    # index_name <- attr(object, "method")
    object <- as.matrix(object)
    methods::callGeneric(object, type = type,
                         diag = diag, upper = upper, lower = lower)
  }
)
