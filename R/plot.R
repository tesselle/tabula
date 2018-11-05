#' @include AllGenerics.R AllClasses.R statistics.R
NULL

# Bar plot =====================================================================
barPlot <- function(object, EPPM = FALSE, center = TRUE, horizontal = FALSE) {
  # Prepare data ---------------------------------------------------------------
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object) %>% factor(levels = unique(.))

  # Build long table from data
  if (EPPM) {
    object <- methods::as(object, "FrequencyMatrix")
    threshold <- threshold(object, method = "EPPM")
  } else {
    threshold <- matrix(data = 0, ncol = ncol(object), nrow = nrow(object),
                        dimnames = dimnames(object))
  }

  scale <- ifelse(methods::is(object, "CountMatrix"), "count", "frequency")
  if (center) {
    object %<>% { rbind(., -.) / 2 }
    threshold %<>% { rbind(., -.) / 2 }
  }

  # Build long table from threshold
  threshold %<>% as.data.frame() %>%
    dplyr::mutate(id = rownames(.), case = rep(row_names, 1 + center)) %>%
    tidyr::gather(key = "type", value = "EPPM", -id, -case, factor_key = TRUE)

  # Build long table from data and join with threshold
  # 'id' is only used for joining then removed
  data <- object %>%
    as.data.frame() %>%
    dplyr::mutate(id = rownames(.), case = rep(row_names, 1 + center)) %>%
    tidyr::gather(key = "type", value = "data", -id, -case,
                  factor_key = TRUE) %>%
    dplyr::inner_join(threshold, by = c("id", "case", "type")) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(data = data - EPPM) %>%
    tidyr::gather(key = "threshold", value = !!scale, -case, -type,
                  factor_key = TRUE)

  # ggplot -------------------------------------------------------------------
  fill <- if (EPPM) "threshold" else NULL
  bertin <- center | horizontal
  rows <- ifelse(bertin, "case", "type")
  cols <- ifelse(bertin, "type", "case")
  coord <- if (bertin) coord_flip() else NULL
  axis <- if (bertin) {
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  } else {
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  }
  ggplot(data = data) +
    facet_grid(stats::reformulate(cols, rows), scales = "free", space = "free_x") +
    geom_col(aes_string(x = "case", y = scale, fill = fill), width = 1,
             position = position_stack(reverse = !center)) +
    coord + axis +
    scale_x_discrete(expand = c(0, 0)) +
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

  # Build long table from data
  if (PVI) {
    object <- methods::as(object, "CountMatrix")
    threshold <- threshold(object, method = "PVI")
  } else {
    threshold <- matrix(data = 0, ncol = ncol(object), nrow = nrow(object),
                        dimnames = dimnames(object))
  }

  scale <- ifelse(methods::is(object, "CountMatrix"), "count", "frequency")

  # Build long table from threshold
  threshold %<>% as.data.frame() %>%
    dplyr::mutate(id = rownames(.), case = row_names) %>%
    tidyr::gather(key = "type", value = "PVI", -id, -case, factor_key = TRUE)

  # Build long table from data and join with threshold
  # 'id' is only used for joining then removed
  data <- object %>%
    as.data.frame() %>%
    dplyr::mutate(id = rownames(.), case = row_names) %>%
    tidyr::gather(key = "type", value = !!scale,
                  -id, -case, factor_key = TRUE) %>%
    dplyr::inner_join(threshold, by = c("id", "case", "type")) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(
      case_num = as.numeric(case),
      type_num = as.numeric(type),
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

  # ggplot ---------------------------------------------------------------------
  ggplot_geom <- if (PVI) {
    background <- if (center) {
      list(geom_tile(aes(x = type_num, y = case_num), fill = "grey50"))
    } else {
      list(geom_tile(aes(x = type_num, y = case_num), fill = "white"),
           geom_rect(
             aes(xmin = type_num - tile2_size, xmax = type_num + tile2_size,
                 ymin = case_num - tile2_size, ymax = case_num + tile2_size,
                 fill = threshold2),
             show.legend = FALSE
           )
      )
    }
    c(background,
      geom_rect(
        aes(xmin = type_num - tile1_size, xmax = type_num + tile1_size,
            ymin = case_num - tile1_size, ymax = case_num + tile1_size,
            fill = threshold),
        show.legend = TRUE
      )
    )
  } else {
    list(geom_tile(aes_string(x = "type_num", y = "case_num", fill = scale)))
  }

  ggplot(data = data) +
    ggplot_geom +
    scale_x_continuous(position = "top", expand = c(0, 0),
                       limits = range(data$type_num) + c(-0.5, 0.5),
                       breaks = unique(data$type_num),
                       labels = unique(data$type)) +
    scale_y_continuous(expand = c(0, 0), trans = "reverse",
                       breaks = unique(data$case_num),
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
      tidyr::gather(key = "type", value = "value", -case, factor_key = TRUE)

    # ggplot -------------------------------------------------------------------
    ggplot(data = data, aes(x = type, y = case, fill = value)) +
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
    tidyr::gather(key = "type", value = "frequency", -case,
                  factor_key = TRUE) %>%
    dplyr::filter(frequency > 0) %>%
    dplyr::group_by(case) %>%
    dplyr::mutate(rank = dplyr::row_number(frequency)) %>%
    dplyr::arrange(rank, .by_group = TRUE) %>%
    dplyr::mutate(rank = rev(rank)) %>%
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
  ggplot(data = data, aes_string(x = "rank", y = "frequency", colour = colour)) +
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
  data <- { 0.8 * object / rowSums(object) } %>%
    as.data.frame() %>%
    dplyr::mutate(case = row_names) %>%
    tidyr::gather(key = "type", value = "frequency", -case, factor_key = TRUE)

  if (!is.null(threshold)) {
    threshold <- match.arg(threshold, c("mean", "median"), several.ok = FALSE)
    fun <- switch (
      threshold,
      mean = base::mean,
      median = stats::median
    )

    data %<>% dplyr::group_by(type) %>%
      dplyr::mutate(
        !!threshold := dplyr::if_else(frequency > fun(frequency),
                                      "above", "below")) %>%
      dplyr::ungroup()
  }

  # ggplot ---------------------------------------------------------------------
  colour <- if (is.null(threshold)) NULL else threshold
  ggplot(data = data, aes(x = type, y = case)) +
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
