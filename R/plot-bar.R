# PLOT BAR
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plotBar-method
#' @aliases plotBertin,CountMatrix-method
setMethod(
  f = "plotBertin",
  signature = signature(object = "CountMatrix"),
  definition = function(object, threshold = NULL, scale = NULL) {
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = rev(unique(.)))

    # Build a long table from data
    data <- object %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "frequency",
                    -.data$case, factor_key = TRUE)

    # Scale variables
    if (is.function(scale)) {
      data %<>%
        dplyr::group_by(.data$type) %>%
        dplyr::mutate(frequency = scale(.data$frequency)) %>%
        dplyr::ungroup()
    }

    # Compute threshold, if any
    if (is.function(threshold)) {
      data %<>%
        dplyr::group_by(.data$type) %>%
        dplyr::mutate(thresh = threshold(.data$frequency)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(threshold = dplyr::if_else(.data$frequency > .data$thresh,
                                                 "above", "below"))
    }

    # ggplot
    aes_plot <- ggplot2::aes(x = .data$case, y = .data$frequency)
    aes_col <- if (is.null(threshold)) NULL else ggplot2::aes(fill = .data$threshold)

    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_col(mapping = aes_col, colour = "black") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$type), scales = "free_y") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.text.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
        strip.background = ggplot2::element_rect(fill = "white")
      ) +
      ggplot2::labs(fill = "Threshold")
  }
)

#' @export
#' @rdname plotBar-method
#' @aliases plotFord,CountMatrix-method
setMethod(
  f = "plotFord",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = FALSE, EPPM = FALSE) {
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = rev(unique(.)))

    # Build a long table from data
    data <- object %>%
      { . / rowSums(.) } %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "data",
                    -.data$case, factor_key = TRUE)

    if (EPPM) {
      # Build long table from threshold
      threshold <- independance(object, method = "EPPM") %>%
        as.data.frame() %>%
        dplyr::mutate(case = row_names) %>%
        tidyr::gather(key = "type", value = "EPPM",
                      -.data$case, factor_key = TRUE)

      # Join data and threshold
      data %<>% dplyr::inner_join(threshold, by = c("case", "type")) %>%
        dplyr::mutate(data = .data$data - .data$EPPM) %>%
        tidyr::gather(key = "threshold", value = "data",
                      -.data$case, -.data$type)
    }

    k <- nrow(data)
    z <- c(rep(1, k), rep(-1, k)) / 2
    data %<>% rbind.data.frame(., .) %>%
      dplyr::mutate(data = .data$data * z)

    # ggplot
    # A function that given the scale limits returns a vector of breaks
    scale_breaks <- function(x) {
      if (max(x) >= 0.2) c(-4:4) * 0.10 else c(-1:1) * 0.05
    }
    # A function that takes the breaks as input and returns labels as output
    scale_labels <- function(x) {
      labs <- scales::percent(x = abs(x), accuracy = 1)
      labs[ceiling(length(x) / 2)] <- ""
      labs
    }

    aes_plot <- ggplot2::aes(x = .data$case, y = .data$data)
    aes_col <- if (EPPM) ggplot2::aes(fill = .data$threshold) else NULL
    ggplot2::ggplot(data = data, mapping = aes_plot) +
      ggplot2::geom_col(mapping = aes_col, width = 1,
                        position = ggplot2::position_stack(reverse = FALSE)) +
      ggplot2::facet_grid(. ~ type, scales = "free_x", space = "free_x") +
      ggplot2::scale_y_continuous(breaks = scale_breaks, labels = scale_labels,
                                  expand = c(0, 0.025)) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.key = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5),
        strip.background = ggplot2::element_rect(fill = "white")) +
      ggplot2::labs(fill = "Value") +
      ggplot2::coord_flip()
  }
)


#' @export
#' @rdname deprecated
#' @aliases plotBar,CountMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = FALSE, EPPM = FALSE,
                        center = TRUE, horizontal = FALSE) {
    .Deprecated(msg = "plotBar is deprecated. Use plotBertin or plotFord instead.")
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = rev(unique(.)))

    # Build long table from data and join with threshold
    # 'id' is only used for joining
    data <- object %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "data",
                    -.data$case, factor_key = TRUE) %>%
      dplyr::group_by(.data$case) %>%
      dplyr::mutate(
        ci = { if (level) confidence(.data$data, level = level) else 0 },
        data = .data$data / sum(abs(.data$data)), # Computes frequencies
        conf_center = .data$data # Center of confidence intervals
      ) %>%
      dplyr::ungroup()

    if (EPPM) {
      # Build long table from threshold
      # 'id' is only used for joining
      threshold <- independance(object, method = "EPPM") %>%
        as.data.frame() %>%
        dplyr::mutate(case = row_names) %>%
        tidyr::gather(key = "type", value = "EPPM",
                      -.data$case, factor_key = TRUE)

      # Join data and with threshold
      data %<>% dplyr::inner_join(threshold, by = c("case", "type")) %>%
        dplyr::mutate(data = .data$data - .data$EPPM) %>%
        tidyr::gather(key = "threshold", value = "data", -.data$conf_center,
                      -.data$case, -.data$type, -.data$ci,
                      factor_key = TRUE) %>%
        dplyr::mutate( # Remove CI on EPPM values
          ci = dplyr::case_when(.data$threshold == "EPPM" ~ 0, TRUE ~ .data$ci)
        )
    }
    if (center) {
      k <- nrow(data)
      z <- c(rep(1, k), rep(-1, k)) / 2
      data %<>% rbind.data.frame(., .) %>%
        dplyr::mutate(data = .data$data * z,
                      ci = .data$ci * z,
                      conf_center = .data$conf_center * z)
    }

    # Rename axis
    data %<>% dplyr::rename(frequency = "data")

    # ggplot
    scale_breaks <- c(-4:4) * 0.10
    fill <- if (EPPM) "threshold" else NULL
    bertin <- center | horizontal
    facets <- if (bertin) ".~type" else "type~."
    coord <- if (bertin) ggplot2::coord_flip() else NULL
    axis <- if (bertin) {
      list(
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank()),
        ggplot2::scale_y_continuous(breaks = scale_breaks,
                                    labels = abs(scale_breaks))
      )
    } else {
      list(
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank()),
        ggplot2::scale_x_discrete(expand = c(0, 0), position = "top"),
        ggplot2::scale_y_continuous(breaks = scale_breaks,
                                    labels = abs(scale_breaks))
      )
    }

    ggplot2::ggplot(data = data) +
      ggplot2::facet_grid(stats::as.formula(facets),
                          scales = "free", space = "free_x") +
      ggplot2::geom_col(ggplot2::aes_string(x = "case", y = "frequency",
                                            fill = fill), width = 1,
               position = ggplot2::position_stack(reverse = !center)) +
      ggplot2::geom_errorbar(ggplot2::aes_string(x = "case",
                                                 ymin = "conf_center-ci",
                                                 ymax = "conf_center+ci"),
                             width = 0, size = 3) +
      coord + axis +
      ggplot2::theme(legend.position = "bottom",
                     panel.spacing = ggplot2::unit(0, "lines"))
  }
)

#' @export
#' @rdname deprecated
#' @aliases plotBar,FrequencyMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, level = FALSE, EPPM = FALSE,
                        center = TRUE, horizontal = FALSE) {
    count <- methods::as(object, "CountMatrix")
    plotBar(count, level = level, EPPM = EPPM,
            center = center, horizontal = horizontal)
  }
)
