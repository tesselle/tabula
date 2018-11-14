#' @include AllGenerics.R AllClasses.R statistics.R
NULL

# Bar plot =====================================================================
#' @export
#' @rdname plotBar-method
#' @aliases plotBar,CountMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = 0.05, EPPM = FALSE,
                        center = TRUE, horizontal = FALSE) {
    # Prepare data -------------------------------------------------------------
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
  definition = function(object, level = 0.05, EPPM = FALSE,
                        center = TRUE, horizontal = FALSE) {
    count <- methods::as(object, "CountMatrix")
    plotBar(count, level = level, EPPM = EPPM,
            center = center, horizontal = horizontal)
  }
)

# Matrix plot ==================================================================
#' @export
#' @rdname plotMatrix-method
#' @aliases plotMatrix,CountMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "CountMatrix"),
  definition = function(object, PVI = FALSE) {
    # Prepare data -------------------------------------------------------------
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

    # ggplot -------------------------------------------------------------------
    fill <- ifelse(PVI, "PVI", "frequency")
    ggplot(data = data) +
      geom_tile(aes_string(x = "x", y = "y", fill = fill)) +
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
            axis.text.x = element_text(angle = 90, hjust = 0),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank()) +
      coord_fixed()
  }
)

# Rank plot ====================================================================
#' @export
#' @rdname plotRank-method
#' @aliases plotRank,CountMatrix-method
setMethod(
  f = "plotRank",
  signature = signature(object = "CountMatrix"),
  definition = function(object, log = NULL, facet = TRUE) {
    freq <- methods::as(object, "FrequencyMatrix")
    plotRank(freq, log = log, facet = facet)
  }
)

#' @export
#' @rdname plotRank-method
#' @aliases plotRank,FrequencyMatrix-method
setMethod(
  f = "plotRank",
  signature = signature(object = "FrequencyMatrix"),
  definition = function(object, log = NULL, facet = TRUE) {
    # Prepare data -------------------------------------------------------------
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))
    # Get number of cases
    n <- length(row_names)

    data <- object %>%
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

    # ggplot -------------------------------------------------------------------
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
)

# Spot plot ====================================================================
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
    # Prepare data -------------------------------------------------------------
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

    # ggplot -------------------------------------------------------------------
    colour <- if (is.null(threshold)) NULL else function_name
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
)
