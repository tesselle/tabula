# DEPRECATED METHODS

# ========================================================================= plot
# ---------------------------------------------------------------------- plotBar
#' @export
#' @rdname deprecated
#' @aliases plotBar,CountMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = FALSE, EPPM = FALSE,
                        center = TRUE, horizontal = FALSE) {
    .Deprecated(
      msg = "plotBar is deprecated. Use plot_bertin or plot_ford instead."
    )
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

# ------------------------------------------------------------------- plotMatrix
#' @export
#' @rdname deprecated
#' @aliases plotMatrix,CountMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "CountMatrix"),
  definition = function(object, PVI = FALSE) {
    .Deprecated(msg = "plotMatrix is deprecated. Use plot_heatmap instead.")
    plot_heatmap(object, PVI = PVI)
  }
)

#' @export
#' @rdname deprecated
#' @aliases plotMatrix,FrequencyMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, PVI = FALSE) {
    .Deprecated(msg = "plotMatrix is deprecated. Use plot_heatmap instead.")
    count <- methods::as(object, "CountMatrix")
    plot_heatmap(count, PVI = PVI)
  }
)

#' @export
#' @rdname deprecated
#' @aliases plotMatrix,IncidenceMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object) {
    .Deprecated(msg = "plotMatrix is deprecated. Use plot_heatmap instead.")
    plot_heatmap(object)
  }
)

# --------------------------------------------------------------------- plotRank
#' @export
#' @rdname deprecated
#' @aliases plotRank,CountMatrix-method
setMethod(
  f = "plotRank",
  signature = signature(object = "CountMatrix"),
  definition = function(object, PVI = FALSE) {
    .Deprecated(msg = "plotRank is deprecated. Use plot_rank instead.")
    plot_rank(object)
  }
)

#' @export
#' @rdname deprecated
#' @aliases plotRank,FrequencyMatrix-method
setMethod(
  f = "plotRank",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, PVI = FALSE) {
    .Deprecated(msg = "plotRank is deprecated. Use plot_rank instead.")
    plot_rank(object)
  }
)

# --------------------------------------------------------------------- plotSpot
#' @export
#' @rdname deprecated
#' @aliases plotSpot,CountMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "CountMatrix"),
  definition = function(object, threshold = NULL) {
    .Deprecated(msg = "plotSpot is deprecated. Use plot_spot instead.")
    freq <- methods::as(object, "FrequencyMatrix")
    plot_spot(freq, threshold = threshold)
  }
)
#' @export
#' @rdname deprecated
#' @aliases plotSpot,FrequencyMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, threshold = NULL) {
    .Deprecated(msg = "plotSpot is deprecated. Use plot_spot instead.")
    plot_spot(object, threshold = threshold)

  }
)
#' @export
#' @rdname deprecated
#' @aliases plotSpot,SimilarityMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "SimilarityMatrix"),
  definition = function(object) {
    .Deprecated(msg = "plotSpot is deprecated. Use plot_spot instead.")
    plot_spot(object)
  }
)
#' @export
#' @rdname deprecated
#' @aliases plotSpot,OccurrenceMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "OccurrenceMatrix"),
  definition = function(object) {
    .Deprecated(msg = "plotSpot is deprecated. Use plot_spot instead.")
    plot_spot(object)
  }
)

# ====================================================================== seriate
#' @export
#' @rdname deprecated
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix", subset = "missing"),
  definition = function(object, method = c("correspondance", "reciprocal"),
                        EPPM = FALSE, margin = c(1, 2), stop = 100, ...) {
    .Deprecated(msg = "seriate is deprecated. Use seriate_reciprocal or seriate_correspondance instead.")
    seriation(object, method = method, EPPM = EPPM, margin = margin,
              stop = stop, ...)
  }
)
#' @export
#' @rdname deprecated
setMethod(
  f = "seriate",
  signature = signature(object = "IncidenceMatrix", subset = "missing"),
  definition = function(object, method = c("correspondance", "reciprocal"),
                        margin = c(1, 2), stop = 100, ...) {
    .Deprecated(msg = "seriate is deprecated. Use seriate_reciprocal or seriate_correspondance instead.")
    seriation(object * 1, method = method, margin = margin, stop = stop, ...)
  }
)
#' @export
#' @rdname deprecated
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix", subset = "BootCA"),
  definition = function(object, subset, margin = c(1, 2), ...) {
    .Deprecated(msg = "seriate is deprecated. Use seriate_correspondance instead.")
    seriate_correspondance(object, subset, margin = margin, ...)
  }
)
