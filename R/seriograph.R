# SERIOGRAPH
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname seriograph
#' @aliases eppm,matrix-method
setMethod(
  f = "eppm",
  signature = signature(object = "matrix"),
  definition = function(object) {
    # Independence
    values <- expected(object)

    # Threshold
    threshold <- (object - values) / rowSums(object)
    threshold[threshold < 0] <- 0

    dimnames(threshold) <- dimnames(object)
    threshold
  }
)

#' @export
#' @rdname seriograph
#' @aliases eppm,data.frame-method
setMethod(
  f = "eppm",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

#' @export
#' @rdname seriograph
#' @aliases seriograph,matrix-method
setMethod(
  f = "seriograph",
  signature = signature(object = "matrix"),
  definition = function(object, weights = FALSE) {
    ## Frequencies
    freq_long <- prepare_ford(object)
    freq_vertex <- prepare_ford_vertex(freq_long, group = "Frequency")

    ## EPPM
    eppm_data <- eppm(object)
    eppm_long <- arkhe::to_long(eppm_data, factor = FALSE)
    eppm_long$x <- freq_long$x
    eppm_long$y <- freq_long$y
    eppm_vertex <- prepare_ford_vertex(eppm_long, group = "EPPM")

    ## Weights
    gg_weights <- NULL
    if (weights) {
      weight_data <- data.frame(value = rowSums(object) / sum(object))
      weight_data$x <- max(freq_vertex$x) + max(weight_data$value) + 0.04
      weight_data$y <- rev(seq_len(nrow(object)))
      weights_vertex <- prepare_ford_vertex(weight_data, group = "Weights")

      gg_weights <- ggplot2::geom_polygon(
        data = weights_vertex,
        fill = "grey"
      )
    }

    ggplot2::ggplot() +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group
        # fill = .data$Value
      ) +
      ggplot2::geom_polygon(data = freq_vertex, fill = "darkgrey") +
      ggplot2::geom_polygon(data = eppm_vertex, fill = "black") +
      gg_weights +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = unique(freq_long$x),
        labels = colnames(object),
        position = "top"
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        breaks = seq_len(nrow(object)),
        labels = rev(rownames(object))
      ) +
      theme_tabula()
  }
)

#' @export
#' @rdname seriograph
#' @aliases seriograph,data.frame-method
setMethod(
  f = "seriograph",
  signature = signature(object = "data.frame"),
  definition = function(object, weights = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, weights = weights)
  }
)
