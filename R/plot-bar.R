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
      print("ok")
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
    colour <- if (is.null(threshold)) NULL else "threshold"

    ggplot(data = data, aes_string(x = "case", y = "frequency")) +
      geom_col(aes_string(fill = colour), colour = "black") +
      scale_x_discrete(position = "top") +
      facet_grid(type ~ ., scales = "free_y") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.key = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            strip.text.y = element_text(angle = 0, hjust = 0),
            strip.background = element_rect(fill = "white"))
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
    colour <- if (EPPM) "threshold" else NULL
    # A function that given the scale limits returns a vector of breaks
    scale_breaks <- function(x) {
      if (max(x) >= 0.2) {
        c(-4:4) * 0.10
      } else {
        c(-1:1) * 0.05
      }
    }
    # A function that takes the breaks as input and returns labels as output
    scale_labels <- function(x) {
      labs <- scales::percent(x = abs(x), accuracy = 1)
      labs[ceiling(length(x) / 2)] <- ""
      labs
    }

    ggplot(data = data, aes_string(x = "case", y = "data")) +
      geom_col(aes_string(fill = colour), width = 1,
               position = position_stack(reverse = FALSE)) +
      facet_grid(. ~ type, scales = "free_x", space = "free_x") +
      scale_y_continuous(breaks = scale_breaks, labels = scale_labels,
                         expand = c(0, 0.025)) +
      theme(axis.title = element_blank(),
            axis.ticks.y = element_blank(),
            legend.key = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            strip.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
            strip.background = element_rect(fill = "white")) +
      coord_flip()
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
    coord <- if (bertin) coord_flip() else NULL
    axis <- if (bertin) {
      list(
        theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank()),
        scale_y_continuous(breaks = scale_breaks, labels = abs(scale_breaks))
      )
    } else {
      list(
        theme(axis.title.x = element_blank(),
              axis.ticks.x = element_blank()),
        scale_x_discrete(expand = c(0, 0), position = "top"),
        scale_y_continuous(breaks = scale_breaks, labels = abs(scale_breaks))
      )
    }

    ggplot(data = data) +
      facet_grid(stats::as.formula(facets), scales = "free", space = "free_x") +
      geom_col(aes_string(x = "case", y = "frequency", fill = fill), width = 1,
               position = position_stack(reverse = !center)) +
      geom_errorbar(aes_string(x = "case", ymin = "conf_center-ci",
                               ymax = "conf_center+ci"),
                    width = 0, size = 3) +
      coord + axis +
      theme(legend.position = "bottom",
            panel.spacing = unit(0, "lines"))
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
