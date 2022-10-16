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
    eppm_long <- arkhe::as_long(eppm_data, factor = FALSE)
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
        fill = "darkgrey"
      )
    }

    ## ggplot
    ## A function that given the scale limits returns a vector of breaks
    scale_breaks <- function(x) {
      if (max(x) >= 0.2) {
        seq(-4, 4, by = 2) * 0.10
      } else {
        seq(-1, 1, by = 1) * 0.05
      }
    }
    ## A function that takes the breaks as input and returns labels as output
    scale_labels <- function(x) {
      labs <- scale_pc(x)
      labs[ceiling(length(x) / 2)] <- "0"
      labs
    }

    ggplot2::ggplot() +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group,
        fill = .data$Value
      ) +
      ggplot2::geom_polygon(data = freq_vertex) +
      ggplot2::geom_polygon(data = eppm_vertex) +
      gg_weights +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = scale_breaks,
        labels = scale_labels,
        sec.axis = ggplot2::sec_axis(
          trans = ~ .,
          breaks = unique(freq_long$x),
          labels = colnames(object)
        )
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
