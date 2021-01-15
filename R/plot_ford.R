# PLOT FORD
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_bar
#' @aliases plot_ford,CountMatrix-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "CountMatrix"),
  definition = function(object, EPPM = FALSE) {
    ## Prepare data
    object_long <- prepare_ford(object, EPPM = EPPM)
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

    aes_fill <- if (EPPM) ggplot2::aes(fill = .data$fill) else NULL
    ford <- ggplot2::ggplot(data = vertex) +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group
      ) +
      aes_fill +
      ggplot2::geom_polygon() +
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
        breaks = rev(seq_len(nrow(object))),
        labels = rownames(object)
      ) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.ticks.x.top = ggplot2::element_blank(),
        axis.text.x.top = ggplot2::element_text(
          angle = 90,
          hjust = 0,
          vjust = 0.5
        ),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::labs(x = "Type", y = "Case", fill = "Value")

    return(ford)
  }
)

#' Prepare data for Ford plot
#' @return A data.frame.
#' @keywords internal
#' @noRd
prepare_ford <- function(x, EPPM = FALSE) {
  ## Relative frequencies
  freq <- arkhe::as_abundance(x)

  ## Adaptive spacing between columns
  padding <- 0.02
  col_max <- apply(X = freq, MARGIN = 2, FUN = max, na.rm = TRUE)
  roll_max <- roll_sum(col_max, n = 2) + padding
  cum_max <- c(0, cumsum(roll_max))

  ## Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_long(freq, factor = TRUE)

  m <- nrow(freq)
  data$x <- rep(cum_max, each = m)
  data$y <- m + 1 - as.integer(data$case) # Reverse levels order
  data$type <- "data"

  if (EPPM) {
    eppm_data <- eppm(x)
    eppm_long <- arkhe::as_long(eppm_data, factor = TRUE)
    eppm_long$x <- data$x
    eppm_long$y <- data$y
    eppm_long$type <- "EPPM"

    data <- rbind(data, eppm_long)
  }

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
      fill = id
    )
  }

  vertex <- do.call(rbind, vertex)
  return(vertex)
}
