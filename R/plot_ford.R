# PLOT FORD
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_bar
#' @aliases plot_ford,matrix-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "matrix"),
  definition = function(object) {
    ## Prepare data
    object_long <- prepare_ford(object)
    vertex <- prepare_ford_vertex(object_long)

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

    ford <- ggplot2::ggplot() +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group
      ) +
      ggplot2::geom_polygon(data = vertex) +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = scale_breaks,
        labels = scale_labels,
        sec.axis = ggplot2::sec_axis(
          trans = ~ .,
          breaks = unique(object_long$x),
          labels = colnames(object)
        )
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        breaks = seq_len(nrow(object)),
        labels = rev(rownames(object))
      ) +
      theme_tabula()

    return(ford)
  }
)

#' @export
#' @rdname plot_bar
#' @aliases plot_ford,CountMatrix-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "CountMatrix"),
  definition = function(object, EPPM = FALSE) {

    if (EPPM) {
      object_long <- prepare_ford(object)
      eppm_data <- eppm(object)
      eppm_long <- arkhe::as_long(eppm_data, factor = FALSE)
      eppm_long$x <- object_long$x
      eppm_long$y <- object_long$y
      eppm_long$type <- "EPPM"
      vertex_eppm <- prepare_ford_vertex(eppm_long)
      gg_eppm <- ggplot2::geom_polygon(
        mapping = ggplot2::aes(fill = .data$value),
        data = vertex_eppm
      )
    } else {
      gg_eppm <- NULL
    }

    ford <- methods::callNextMethod(object) + gg_eppm
    return(ford)
  }
)

#' Prepare data for Ford plot
#' @return A data.frame.
#' @keywords internal
#' @noRd
prepare_ford <- function(x) {
  ## Relative frequencies
  freq <- arkhe::as_composition(x)

  ## Adaptive spacing between columns
  padding <- 0.02
  col_max <- apply(X = freq, MARGIN = 2, FUN = max, na.rm = TRUE)
  roll_max <- roll_sum(col_max, n = 2) + padding
  cum_max <- c(0, cumsum(roll_max))

  ## Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_long(freq, factor = TRUE)

  m <- nrow(freq)
  data$x <- rep(cum_max, each = m)
  data$y <- m + 1 - as.integer(data$row) # Reverse levels order
  data$type <- "frequency"

  return(data)
}

prepare_ford_vertex <- function(x) {
  n <- nrow(x)
  vertex <- vector(mode = "list", length = n)

  ## Compute polygon vertices
  ## Each row gives one vertex of a polygon
  for (i in seq_len(n)) {
    temp <- x[i, ]
    id <- temp$type
    vertex[[i]] <- data.frame(
      x = temp$x + temp$value * c(-1, 1, 1, -1),
      y = temp$y + 0.5 * c(1, 1, -1, -1),
      group = paste0(id, i),
      value = id
    )
  }

  vertex <- do.call(rbind, vertex)
  return(vertex)
}
