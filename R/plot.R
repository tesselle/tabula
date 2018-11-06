#' @include AllGenerics.R AllClasses.R statistics.R
NULL

# Bar plot =====================================================================
barPlot <- function(object, EPPM = FALSE, center = TRUE, horizontal = FALSE) {
  # Prepare data ---------------------------------------------------------------
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object) %>% factor(levels = rev(unique(.)))

  # Build long table from data and join with threshold
  # 'id' is only used for joining
  data <- object %>%
    { . / rowSums(.) } %>% # Plot frequency
    { if (center) rbind.data.frame(., -.) / 2 else as.data.frame(.) } %>%
    dplyr::mutate(id = rownames(.), case = rep(row_names, 1 + center)) %>%
    tidyr::gather(key = "type", value = "data",
                  -.data$id, -.data$case, factor_key = TRUE)

  if (EPPM) {
    # Build long table from threshold
    # 'id' is only used for joining
    threshold <- threshold(object, method = "EPPM") %>%
      { . / rowSums(object) } %>%
      { if (center) rbind.data.frame(., -.) / 2 else as.data.frame(.) } %>%
      dplyr::mutate(id = rownames(.), case = rep(row_names, 1 + center)) %>%
      tidyr::gather(key = "type", value = "EPPM",
                    -.data$id, -.data$case, factor_key = TRUE)

    # Join data and with threshold
    data %<>% dplyr::inner_join(threshold, by = c("id", "case", "type")) %>%
      dplyr::mutate(data = .data$data - .data$EPPM) %>%
      tidyr::gather(key = "threshold", value = "data",
                    -.data$id, -.data$case, -.data$type, factor_key = TRUE)
  }

  # Rename axis
  data %<>% dplyr::rename(frequency = "data")

  # ggplot -------------------------------------------------------------------
  fill <- if (EPPM) "threshold" else NULL
  bertin <- center | horizontal
  facets <- if (bertin) ".~type" else "type~."
  coord <- if (bertin) coord_flip() else NULL
  axis <- if (bertin) {
    list(
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank())
    )
  } else {
    list(
      theme(axis.title.x = element_blank(),
            axis.ticks.x = element_blank()),
      scale_x_discrete(expand = c(0, 0), position = "top")
    )
  }

  ggplot(data = data) +
    facet_grid(stats::as.formula(facets), scales = "free", space = "free_x") +
    geom_col(aes_string(x = "case", y = "frequency", fill = fill), width = 1,
             position = position_stack(reverse = !center)) +
    coord + axis +
    theme(legend.position = "bottom",
          panel.spacing = unit(0, "lines"))
}

#' @export
#' @rdname plotBar-method
#' @aliases plotBar,CountMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "CountMatrix"),
  definition = barPlot
)

#' @export
#' @rdname plotBar-method
#' @aliases plotBar,FrequencyMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "FrequencyMatrix"),
  definition = barPlot
)

# Matrix plot ==================================================================
matrixPlot <- function(object, PVI = FALSE, center = FALSE) {
  # Prepare data ---------------------------------------------------------------
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object) %>% factor(levels = unique(.))

  # Build long table from data and join with threshold
  # 'id' is only used for joining
  data <- object %>%
    { . / rowSums(.) } %>%
    as.data.frame() %>%
    dplyr::mutate(id = rownames(.), case = row_names) %>%
    tidyr::gather(key = "type", value = "frequency",
                  -.data$id, -.data$case, factor_key = TRUE)

  if (PVI) {
    # Coerce to count data for PVI computation
    object <- methods::as(object, "CountMatrix")

    # Build long table from threshold
    # 'id' is only used for joining
    threshold <- threshold(object, method = "PVI") %>%
      as.data.frame() %>%
      dplyr::mutate(id = rownames(.), case = row_names) %>%
      tidyr::gather(key = "type", value = "PVI",
                    -.data$id, -.data$case, factor_key = TRUE)

    # Join data and with threshold
    data %<>% dplyr::inner_join(threshold, by = c("id", "case", "type")) %>%
      dplyr::mutate(
        case_num = as.numeric(.data$case),
        type_num = as.numeric(.data$type),
        threshold = factor(dplyr::if_else(PVI > 1, "above", "below")),
        tile1_size = dplyr::case_when(
          PVI > 2 ~ 1,
          PVI > 1 ~ PVI - 1,
          TRUE ~ {if (center) 1 - PVI else PVI }) / 2) %>%
      dplyr::mutate(
        threshold2 = dplyr::case_when(
          threshold == "above" ~ "below",
          threshold == "below" ~ "above"),
        tile2_size = dplyr::case_when(
          threshold == "above" ~ 0.5,
          threshold == "below" ~ 0))
  }

  # Tile centers
  data %<>% dplyr::mutate(
    x = as.numeric(.data$type),
    y = as.numeric(.data$case)
  )

  # ggplot ---------------------------------------------------------------------
  ggplot_geom <- if (PVI) {
    background <- if (center) {
      list(geom_tile(aes_string(x = "x", y = "y"), fill = "grey50"))
    } else {
      list(geom_tile(aes_string(x = "x", y = "y"), fill = "white"),
           geom_rect(
             aes_string(
               xmin = "x - tile2_size", xmax = "x + tile2_size",
               ymin = "y - tile2_size", ymax = "y + tile2_size",
               fill = "threshold2"),
             show.legend = FALSE
           )
      )
    }
    c(background,
      geom_rect(
        aes_string(
          xmin = "x - tile1_size", xmax = "x + tile1_size",
          ymin = "y - tile1_size", ymax = "y + tile1_size",
          fill = "threshold"),
        show.legend = TRUE
      )
    )
  } else {
    list(geom_tile(aes_string(x = "x", y = "y", fill = "frequency")))
  }

  ggplot(data = data) +
    ggplot_geom +
    scale_x_continuous(position = "top", expand = c(0, 0),
                       limits = range(data$x) + c(-0.5, 0.5),
                       breaks = unique(data$x),
                       labels = unique(data$type)) +
    scale_y_continuous(expand = c(0, 0), trans = "reverse",
                       breaks = unique(data$y),
                       labels = unique(data$case)) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()) +
    coord_fixed()
}

#' @export
#' @rdname plotMatrix-method
#' @aliases plotMatrix,CountMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "CountMatrix"),
  definition = matrixPlot
)

#' @export
#' @rdname plotMatrix-method
#' @aliases plotMatrix,FrequencyMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "FrequencyMatrix"),
  definition = matrixPlot
)

#' @export
#' @rdname plotMatrix-method
#' @aliases plotMatrix,IncidenceMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object) {
    # Prepare data -------------------------------------------------------------
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))

    # Build long table from data and join with threshold
    data <- object %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "value",
                    -.data$case, factor_key = TRUE)

    # ggplot -------------------------------------------------------------------
    ggplot(data = data, aes_string(x = "type", y = "case", fill = "value")) +
      geom_tile(color = "black") +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev(levels(data$case))) +
      theme(axis.ticks = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank()) +
      coord_fixed()
  }
)

# Rank plot ====================================================================
rankPlot <- function(object, log = NULL, facet = TRUE) {
  # Prepare data ---------------------------------------------------------------
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object) %>% factor(levels = unique(.))
  # Get number of cases
  n <- length(row_names)

  data <- { object / rowSums(object) } %>%
    as.data.frame() %>%
    dplyr::mutate(case = row_names) %>%
    tidyr::gather(key = "type", value = "frequency", -.data$case,
                  factor_key = TRUE) %>%
    dplyr::filter(.data$frequency > 0) %>%
    dplyr::group_by(.data$case) %>%
    dplyr::mutate(rank = dplyr::row_number(.data$frequency)) %>%
    dplyr::arrange(rank, .by_group = TRUE) %>%
    dplyr::mutate(rank = rev(.data$rank)) %>%
    dplyr::ungroup()

  # ggplot ---------------------------------------------------------------------
  log_x <- log_y <- NULL
  if (!is.null(log)) {
    if (log == "x" | log == "xy" | log == "yx") log_x <- scale_x_log10()
    if (log == "y" | log == "xy" | log == "yx") log_y <- scale_y_log10()
  }
  if (facet) {
    colour <- NULL
    facet <- facet_wrap(.~case, ncol = n)
  } else {
    colour <- "case"
    facet <- NULL
  }
  ggplot(data = data, aes_string(x = "rank", y = "frequency",
                                 colour = colour)) +
    geom_point() + geom_line() +
    log_x + log_y + facet
}

#' @export
#' @rdname plotRank-method
#' @aliases plotRank,CountMatrix-method
setMethod(
  f = "plotRank",
  signature = signature(object = "CountMatrix"),
  definition = rankPlot
)

#' @export
#' @rdname plotRank-method
#' @aliases plotRank,FrequencyMatrix-method
setMethod(
  f = "plotRank",
  signature = signature(object = "FrequencyMatrix"),
  definition = rankPlot
)

# Spot plot ====================================================================
spotPlot <- function(object, threshold = NULL) {
  # Prepare data ---------------------------------------------------------------
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object) %>% factor(levels = unique(.))

  # Build long table from data
  data <- object %>%
    { 0.8 * . / rowSums(.) } %>%
    as.data.frame() %>%
    dplyr::mutate(case = row_names) %>%
    tidyr::gather(key = "type", value = "frequency",
                  -.data$case, factor_key = TRUE)

  if (!is.null(threshold)) {
    threshold <- match.arg(threshold, c("mean", "median"), several.ok = FALSE)
    fun <- switch (
      threshold,
      mean = base::mean,
      median = stats::median
    )

    data %<>% dplyr::group_by(.data$type) %>%
      dplyr::mutate(
        !!threshold := dplyr::if_else(.data$frequency > fun(.data$frequency),
                                      "above", "below")) %>%
      dplyr::ungroup()
  }

  # ggplot ---------------------------------------------------------------------
  colour <- if (is.null(threshold)) NULL else threshold
  ggplot(data = data, aes_string(x = "type", y = "case")) +
    geom_point(aes(size = 1), colour = "black") +
    geom_point(aes(size = 0.8), colour = "white") +
    geom_point(aes_string(size = "frequency", colour = colour)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(data$case))) +
    scale_size_area() +
    theme(axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.key = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank()) +
    coord_fixed()
}

#' @export
#' @rdname plotSpot-method
#' @aliases plotSpot,CountMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "CountMatrix"),
  definition = spotPlot
)

#' @export
#' @rdname plotSpot-method
#' @aliases plotSpot,FrequencyMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "FrequencyMatrix"),
  definition = spotPlot
)
