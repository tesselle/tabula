#' @include AllGenerics.R AllClasses.R statistics.R
NULL

# Date plot ====================================================================
#' @export
#' @rdname plotDate-method
#' @aliases plotDate,DateModel-method
setMethod(
  f = "plotDate",
  signature = c(object = "DateModel"),
  definition = function(object, type = c("event", "accumulation"),
                        select = 1, n = 500) {
    # Selection
    type <- match.arg(type, several.ok = TRUE)
    cases <- object@rows$id
    index <- if (is.character(select)) {
      which(cases %in% select)
    } else {
      as.numeric(select)
    }
    # Validation
    if (length(index) == 0 | max(index) > length(cases))
      stop("wrong selection")

    # Get data
    row_dates <- object@rows$estimation
    row_lower <- object@rows$earliest
    row_upper <- object@rows$latest
    row_errors <- object@rows$error
    col_dates <- object@columns$estimation
    col_errors <- object@columns$error
    date_range <- seq(from = min(row_lower), to = max(row_upper), length.out = n)

    plot_event <- plot_accumulation <- plot_facet <- NULL

    # Event time
    if ("event" %in% type) {
      date_event <- mapply(function(mean, sd, x) { stats::dnorm(x, mean, sd) },
                           mean = row_dates[index], sd = row_errors[index],
                           MoreArgs = list(x = date_range), SIMPLIFY = TRUE)
      colnames(date_event) <- cases[index]

      row_data <- cbind.data.frame(date = date_range, date_event) %>%
        tidyr::gather(key = "assemblage", value = "density", -date)

      plot_event <- geom_line(data = row_data,
                              mapping = aes_string(color = "assemblage"))
    }

    # Accumulation time
    if ("accumulation" %in% type) {
      # Weighted sum of the fabric dates
      counts <- object@counts[index, , drop = FALSE]
      freq <- counts / rowSums(counts)

      col_density <- mapply(function(mean, sd, x) { stats::dnorm(x, mean, sd) },
                            mean = col_dates, sd = col_errors,
                            MoreArgs = list(x = date_range), SIMPLIFY = TRUE)

      date_acc <- apply(X = freq, MARGIN = 1, FUN = function(x, density) {
        colSums(t(density) * as.numeric(x))
      }, density = col_density)

      col_data <- cbind.data.frame(date = date_range, date_acc) %>%
        tidyr::gather(key = "assemblage", value = "density", -date)

      plot_accumulation <- geom_area(
        data = col_data, fill = "darkgrey", color = "darkgrey", alpha = 0.7)
        # mapping = aes(fill = assemblage, color = assemblage))
    }

    if (length(index) > 1) {
      plot_facet <- facet_wrap(. ~ assemblage, nrow = length(index),
                               scales = "free_y")
    }

    ggplot(mapping = aes_string(x = "date", y = "density")) +
      plot_accumulation + plot_event + plot_facet +
      labs(x = "Date (years)", y = "Density",
           fill = "Assemblage", color = "Assemblage")
  }
)

# @export
# @rdname plotDate-method
# @aliases plotDate,DateModel-method
# setMethod(
#   f = "plotDate",
#   signature = signature(object = "DateModel"),
#   definition = function(object, select = 1, sort = "dsc") {
#     # Selection
#     cases <- object@rows$id
#     index <- if (is.character(select)) {
#       which(cases %in% select)
#     } else {
#       as.numeric(select)
#     }
#
#     # Validation
#     if (length(index) == 0 | max(index) > length(cases))
#       stop("wrong selection")
#
#     date_data <- cbind.data.frame(
#       id = as.factor(cases),
#       event = object@rows$estimation,
#       accumulation = object@accumulation$date,
#       y = as.numeric(as.factor(cases)),
#       xmin = object@rows$earliest,
#       xmax = object@rows$latest
#     )[index, ]
#
#     if (!is.null(sort)) {
#       sort <- match.arg(sort, choices = c("asc", "dsc"), several.ok = FALSE)
#       date_data <- switch (
#         sort,
#         asc = dplyr::arrange(date_data, .data$event),
#         dsc = dplyr::arrange(date_data, dplyr::desc(.data$event))
#       )
#       date_data %<>% dplyr::mutate(y = 1:nrow(.))
#     }
#
#     date_data %<>% tidyr::gather(key = "model", value = "date",
#                                  -.data$id, -.data$y, -.data$xmin, -.data$xmax)
#
#     ggplot(mapping = aes_string(color = "model", fill = "model")) +
#       geom_errorbarh(data = subset(date_data, date_data$model == "event"),
#                      mapping = aes_string(xmin = "xmin", xmax = "xmax", y = "y"),
#                      height = 0.5) +
#       geom_point(data = date_data,
#                  mapping = aes_string(x = "date", y = "y", shape = "model"),
#                  size = 2) +
#       scale_y_continuous(breaks = date_data$y, labels = date_data$id) +
#       scale_shape_manual(values = c("event" = 21, "accumulation" = 23)) +
#       labs(x = "Date (years)", y = "",
#            color = "Model", fill = "Model", shape = "Model")
#   }
# )

#' @export
#' @rdname plotBar-method
#' @aliases plotBar,CountMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "CountMatrix"),
  definition = function(object, level = FALSE, EPPM = FALSE,
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

# Matrix plot ==================================================================
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
    ggplot(data = data) +
      geom_tile(aes_string(x = "x", y = "y", fill = fill)) +
      scale_x_continuous(position = "top", expand = c(0, 0),
                         limits = range(data$x) + c(-0.5, 0.5),
                         breaks = unique(data$x),
                         labels = unique(data$type)) +
      scale_y_continuous(expand = c(0, 0), trans = "reverse",
                         breaks = unique(data$y),
                         labels = unique(data$case)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0),
            axis.ticks = element_blank(),
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
    # Prepare data
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

    # ggplot
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
