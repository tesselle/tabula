# PLOT DICE-LERASS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_diceleraas
#' @aliases plot_diceleraas,matrix-method
setMethod(
  f = "plot_diceleraas",
  signature = signature(object = "matrix"),
  definition = function(object) {
    ## Prepare data
    object <- t(object)
    object[object == 0] <- NA

    ## Mean
    moy <- rowMeans(object, na.rm = TRUE)
    ## Standard deviation
    ec <- apply(X = object, MARGIN = 1, FUN = stats::sd, na.rm = TRUE)
    ## Standard error
    se <- ec / sqrt(rowSums(!is.na(object)))
    ## Range
    ran <- t(apply(X = object, MARGIN = 1, FUN = range, na.rm = TRUE))
    colnames(ran) <- c("min", "max")

    data <- data.frame(
      y = seq_len(nrow(object)),
      mean = moy,
      sd = ec,
      se = se,
      ran
    )

    se_data <- data.frame(x = se * 2, y = seq_len(nrow(object)), value = moy)
    se_vertex <- prepare_dice_vertex(se_data[!is.na(se_data$x), ])

    sd_data <- data.frame(x = ec, y = seq_len(nrow(object)), value = moy)
    sd_vertex <- prepare_dice_vertex(sd_data[!is.na(sd_data$x), ])

    diceleraas <- ggplot2::ggplot(data) +
      ggplot2::geom_polygon(
        data = sd_vertex,
        mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
        fill = "white", colour = "white"
      ) +
      ggplot2::geom_path(
        data = sd_vertex,
        mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
        colour = "black"
      ) +
      ggplot2::geom_polygon(
        data = se_vertex,
        mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
        fill = "black"
      ) +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(x = .data$mean, y = .data$y,
                               xend = .data$mean, yend = .data$y - 0.5)
      ) +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(x = .data$min, y = .data$y,
                               xend = .data$max, yend = .data$y)
      ) +
      ggplot2::scale_x_continuous(name = "Count") +
      scale_y_matrix(object, name = "Type")

    return(diceleraas)
  }
)

#' @export
#' @rdname plot_diceleraas
#' @aliases plot_diceleraas,data.frame-method
setMethod(
  f = "plot_diceleraas",
  signature = signature(object = "data.frame"),
  definition = function(object) {
    object <- data.matrix(object)
    methods::callGeneric(object)
  }
)

prepare_dice_vertex <- function(x, group = "data") {
  n <- nrow(x)
  vertex <- vector(mode = "list", length = n)

  ## Compute polygon vertices
  ## Each row gives one vertex of a polygon
  for (i in seq_len(n)) {
    temp <- x[i, ]
    vertex[[i]] <- data.frame(
      x = temp$x * c(-1, -1, 1, 1) + temp$value,
      y = temp$y + (1 / 3) * c(0, -1, -1, 0),
      group = paste0(group, i),
      Value = group
    )
  }

  vertex <- do.call(rbind, vertex)
  return(vertex)
}
