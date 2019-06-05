# PLOT MATRIX
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plotMatrix-method
#' @aliases plotMatrix,CountMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "CountMatrix"),
  definition = function(object, PVI = FALSE) {
    # Prepare data
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))

    if (PVI) {
      # Coerce to count data for PVI computation
      object <- methods::as(object, "CountMatrix")

      # Build long table from threshold
      data <- independance(object, method = "PVI") %>%
        as.data.frame() %>%
        dplyr::mutate(case = row_names) %>%
        tidyr::gather(key = "type", value = "PVI",
                      -.data$case, factor_key = TRUE)
    } else {
      # Build long table from data
      data <- object %>%
        { . / rowSums(.) } %>%
        as.data.frame() %>%
        dplyr::mutate(case = row_names) %>%
        tidyr::gather(key = "type", value = "frequency",
                      -.data$case, factor_key = TRUE)
    }

    # Tile centers
    data %<>% dplyr::mutate(
      x = as.numeric(.data$type),
      y = as.numeric(.data$case)
    )

    # ggplot
    fill <- ifelse(PVI, "PVI", "frequency")
    aes_plot <- ggplot2::aes(x = .data$x, y = .data$y, fill = .data[[fill]])
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_tile() +
      ggplot2::scale_x_continuous(
        position = "top",
        expand = c(0, 0),
        limits = range(data$x) + c(-0.5, 0.5),
        breaks = unique(data$x),
        labels = unique(data$type)) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        trans = "reverse",
        breaks = unique(data$y),
        labels = unique(data$case)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(fill = fill) +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname plotMatrix-method
#' @aliases plotMatrix,FrequencyMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, PVI = FALSE) {
    count <- methods::as(object, "CountMatrix")
    plotMatrix(count, PVI = PVI)
  }
)

#' @export
#' @rdname plotMatrix-method
#' @aliases plotMatrix,IncidenceMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object) {
    # Prepare data
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))

    # Build long table from data and join with threshold
    data <- object %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "value",
                    -.data$case, factor_key = TRUE)

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$type, y = .data$case, fill = .data$value)
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::scale_y_discrete(limits = rev(levels(data$case))) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(fill = "Value") +
      ggplot2::coord_fixed()
  }
)
