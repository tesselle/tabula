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

    # Build long table from data
    data <- { object * 0.8 } %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "frequency",
                    -.data$case, factor_key = TRUE)

    if (is.function(threshold)) {
      function_name <- as.character(substitute(threshold))
      function_name <- ifelse(length(function_name) > 1,
                              function_name[[3]], function_name)

      data %<>% dplyr::group_by(.data$type) %>%
        dplyr::mutate(thresh = threshold(.data$frequency)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          !!function_name := dplyr::if_else(.data$frequency > .data$thresh,
                                            "above", "below"))
    }

    # ggplot
    colour <- if (is.null(threshold)) NULL else function_name
    ggplot(data = data, aes_string(x = "type", y = "case")) +
      geom_point(aes(size = 1), colour = "black", show.legend = FALSE) +
      geom_point(aes(size = 0.8), colour = "white", show.legend = FALSE) +
      geom_point(aes_string(size = "frequency", colour = colour)) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev(levels(data$case))) +
      scale_size_area() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.key = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank()) +
      coord_fixed()
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
    ggplot(data = data, mapping = aes_string(x = "type", y = "case")) +
      geom_point(aes_string(size = "max"), colour = "black", show.legend = FALSE) +
      geom_point(aes_string(size = "max * 0.8"), colour = "white", show.legend = FALSE) +
      geom_point(aes_string(size = "similarity", color = "similarity")) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev(levels(data$case))) +
      scale_size_area() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.key = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank()) +
      coord_fixed()
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
    ggplot(data = data, mapping = aes_string(x = "type", y = "case")) +
      geom_point(aes(size = 1), colour = "black", show.legend = FALSE) +
      geom_point(aes(size = 0.8), colour = "white", show.legend = FALSE) +
      geom_point(aes_string(size = "occurrence", color = "occurrence")) +
      scale_x_discrete(position = "top", limits = row_names) +
      scale_y_discrete(limits = rev(row_names)) +
      scale_size_area() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.key = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank()) +
      coord_fixed()
  }
)
