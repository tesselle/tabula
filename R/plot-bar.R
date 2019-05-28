# PLOT BAR
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plotBar-method
#' @aliases plotBar,CountMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = FALSE, EPPM = FALSE,
                        center = TRUE, horizontal = FALSE) {
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
#' @rdname plotBar-method
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
