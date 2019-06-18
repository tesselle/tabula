# PLOT SPOT
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plotSpot-method
#' @aliases plotSpot,CountMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "CountMatrix"),
  definition = function(object, threshold = NULL) {
    freq <- methods::as(object, "FrequencyMatrix")
    plotSpot(freq, threshold = threshold)
  }
)

#' @export
#' @rdname plotSpot-method
#' @aliases plotSpot,FrequencyMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, threshold = NULL) {
    # Prepare data
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))

    # Build a long table from data
    data <- object %>% #{ object * 0.8 } %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "frequency",
                    -.data$case, factor_key = TRUE)

    if (is.function(threshold)) {
      data %<>%
        dplyr::group_by(.data$type) %>%
        dplyr::mutate(thresh = threshold(.data$frequency)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(threshold = dplyr::if_else(.data$frequency > .data$thresh,
                                                 "above", "below"))
    }

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$type, y = .data$case)
    aes_point <- if (is.null(threshold)) {
      ggplot2::aes(size = .data$frequency)
    } else {
      ggplot2::aes(size = .data$frequency, colour = .data$threshold)
    }

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = 1), colour = "black",
                          show.legend = FALSE) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = 0.8), colour = "white",
                          show.legend = FALSE) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::scale_y_discrete(limits = rev(levels(data$case))) +
      ggplot2::scale_size_area() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(colour = "Threshold", size = "Frequency") +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname plotSpot-method
#' @aliases plotSpot,SimilarityMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "SimilarityMatrix"),
  definition = function(object) {
    # Prepare data
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))
    max_value <- unique(diag(object))
    index_name <- object[["method"]]

    # Replace lower part and diagonal values with 0
    clean <- object
    # clean[lower.tri(clean, diag = TRUE)] <- 0

    # Build long table from data
    data <- clean %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "similarity",
                    -.data$case, factor_key = TRUE)  %>%
      dplyr::filter(.data$type != .data$case) %>%
      dplyr::mutate(similarity = similarity * 0.8,
                    max = max_value) %>%
      stats::na.omit()

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$type, y = .data$case)
    aes_point <- ggplot2::aes(size = .data$similarity,
                              colour = .data$similarity)

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = .data$max),
                          colour = "black", show.legend = FALSE) +
      ggplot2::geom_point(mapping = ggplot2::aes(size = .data$max * 0.8),
                          colour = "white", show.legend = FALSE) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::scale_y_discrete(limits = rev(levels(data$case))) +
      ggplot2::scale_size_area() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(colour = index_name, size = index_name) +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname plotSpot-method
#' @aliases plotSpot,OccurrenceMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "OccurrenceMatrix"),
  definition = function(object) {
    # Prepare data
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))

    # Build long tables from data
    data <- { object * 0.8 } %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "occurrence",
                    -.data$case, factor_key = TRUE) %>%
      dplyr::filter(.data$type != .data$case)

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$type, y = .data$case)
    aes_point <- ggplot2::aes(size = .data$occurrence,
                              colour = .data$occurrence)

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_point(ggplot2::aes(size = 1), colour = "black",
                          show.legend = FALSE) +
      ggplot2::geom_point(ggplot2::aes(size = 0.8), colour = "white",
                          show.legend = FALSE) +
      ggplot2::geom_point(mapping = aes_point) +
      ggplot2::scale_x_discrete(position = "top", limits = row_names) +
      ggplot2::scale_y_discrete(limits = rev(row_names)) +
      ggplot2::scale_size_area() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(colour = "Co-occurrence", size = "Co-occurrence") +
      ggplot2::coord_fixed()
  }
)
