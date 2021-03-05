# PLOT BERTIN
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_bar
#' @aliases plot_bertin,matrix-method
setMethod(
  f = "plot_bertin",
  signature = signature(object = "matrix"),
  definition = function(object, threshold = NULL, scale = NULL) {
    ## Prepare data
    object_long <- prepare_bertin(object, threshold = threshold, scale = scale)
    vertex <- prepare_bertin_vertex(object_long)

    ## ggplot
    if (is.null(threshold)) {
      aes_fill <- NULL
    } else {
      aes_fill <- ggplot2::aes(fill = .data$fill)
    }
    bertin <- ggplot2::ggplot(data = vertex) +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group
      ) +
      aes_fill +
      ggplot2::geom_polygon(colour = "black") +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = seq_len(nrow(object)),
        labels = rownames(object),
        position = "top"
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        breaks = seq_len(ncol(object)) + 0.5,
        labels = rev(colnames(object)),
        position = "right"
      ) +
      theme_tabula()

    return(bertin)
  }
)

#' Prepare data for Bertin plot
#' @return A data.frame.
#' @keywords internal
#' @noRd
prepare_bertin <- function(x, threshold = NULL, scale = NULL) {
  ## Scale variables
  if (is.function(scale)) {
    x <- apply(X = x, MARGIN = 2, FUN = scale)
  }

  ## Build a long table for ggplot2 (preserve original ordering)
  data <- arkhe::as_long(x, factor = TRUE)

  ## Compute threshold, if any
  if (is.function(threshold)) {
    thr <- apply(X = x, MARGIN = 2, FUN = threshold)
    thr <- matrix(data = thr, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
    thr <- ifelse(x > thr, "above", "below")
    data$threshold <- as.vector(thr)
  } else {
    data$threshold <- NA
  }

  ## /!\ Bertin plot flips x and y axis /!\
  m <- ncol(x)
  data$x <- as.integer(data$row)
  data$y <- m + 1 - as.integer(data$column) # Reverse levels order
  if (!is.function(scale)) { data$value <- scale_01(data$value) }

  return(data)
}

prepare_bertin_vertex <- function(x) {
  n <- nrow(x)
  vertex <- vector(mode = "list", length = n)

  ## Compute polygon vertices
  ## Each row gives one vertex of a polygon
  for (i in seq_len(n)) {
    temp <- x[i, ]
    vertex[[i]] <- data.frame(
      x = temp$x + 0.45 * c(-1, 1, 1, -1),
      y = temp$y + temp$value * c(0.9, 0.9, 0, 0),
      group = i,
      fill = temp$threshold
    )
  }

  vertex <- do.call(rbind, vertex)
  return(vertex)
}
