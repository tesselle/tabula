# PLOT FORD
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_ford
#' @aliases plot_ford,matrix-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "matrix"),
  definition = function(object, EPPM = FALSE) {
    ## /!\ Deprecate EPPM /!\
    if (EPPM) {
      warning("Argument 'EEPM' is defunct; please use 'seriograph()' instead.",
              call. = FALSE)
    }

    ## Prepare data
    object_long <- prepare_ford(object)
    vertex <- prepare_ford_vertex(object_long)

    ggplot2::ggplot() +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group
      ) +
      ggplot2::geom_polygon(data = vertex, fill = "darkgrey") +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = unique(object_long$x),
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
#' @rdname plot_ford
#' @aliases plot_ford,data.frame-method
setMethod(
  f = "plot_ford",
  signature = signature(object = "data.frame"),
  definition = function(object, EPPM = FALSE) {
    object <- data.matrix(object)
    methods::callGeneric(object, EPPM = EPPM)
  }
)

#' Prepare data for Ford plot
#' @return A data.frame.
#' @keywords internal
#' @noRd
prepare_ford <- function(x, padding = 0.04) {
  ## Relative frequencies
  freq <- x / rowSums(x)

  ## Adaptive spacing between columns
  col_max <- apply(X = freq, MARGIN = 2, FUN = max, na.rm = TRUE)
  roll_max <- roll_sum(col_max, n = 2) + padding
  cum_max <- c(0, cumsum(roll_max))

  ## Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::to_long(freq, factor = TRUE)

  m <- nrow(freq)
  data$x <- rep(cum_max, each = m)
  data$y <- m + 1 - as.integer(data$row) # Reverse levels order

  return(data)
}

prepare_ford_vertex <- function(x, group = "data") {
  n <- nrow(x)
  vertex <- vector(mode = "list", length = n)

  ## Compute polygon vertices
  ## Each row gives one vertex of a polygon
  for (i in seq_len(n)) {
    temp <- x[i, ]
    vertex[[i]] <- data.frame(
      x = temp$x + temp$value * c(-1, 1, 1, -1),
      y = temp$y + 0.5 * c(1, 1, -1, -1),
      group = paste0(group, i),
      Value = group
    )
  }

  vertex <- do.call(rbind, vertex)
  return(vertex)
}

ford_scalebar <- function() {
  list(
    ggplot2::geom_errorbarh(
      mapping = ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, y = .data$y),
      data = data.frame(
        xmin = -0.2,
        xmax = 0.2,
        y = 0
      ),
      height = 0.2,
      inherit.aes = FALSE
    )
    # ggplot2::annotate("text", x = 0, y = -0.3, label = "20%",
    #                   hjust = 0.5, vjust = 0)
  )
}
