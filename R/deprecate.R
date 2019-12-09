# DEPRECATED METHODS

# ======================================================================= refine
#' @export
#' @rdname deprecated
#' @aliases refine,CountMatrix-method
setMethod(
  f = "refine",
  signature = signature(object = "CountMatrix"),
  definition = function(object, cutoff, n = 1000, axes = c(1, 2), ...) {
    .Deprecated(msg = "refine is deprecated. Use refine_seriation instead.")
    refine_seriation(object, cutoff, n, axes, ...)
  }
)
#' @export
#' @rdname deprecated
#' @aliases refine,DateModel-method
setMethod(
  f = "refine",
  signature = signature(object = "DateModel"),
  definition = function(object, method = c("jackknife", "bootstrap"),
                        n = 1000, ...) {
    .Deprecated(msg = "refine is deprecated. Use refine_dates instead.")
    refine_dates(object, method, n, ...)
  }
)

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
    row_names <- rownames(object)
    row_names <- factor(x = row_names, levels = unique(row_names))

    # Build long table from data and join with threshold
    # 'id' is only used for joining
    data_stacked <- utils::stack(as.data.frame(object))
    data <- cbind.data.frame(row_names, data_stacked)
    colnames(data) <- c("case", "data", "type")
    # Preserves the original ordering of the columns
    data$type <- factor(data$type, levels = unique(data$type))

    data <- by(
      data,
      INDICES = data$case,
      FUN = function(x, alpha) {
        ci <- if (alpha) confidence_proportion(x$data, alpha = alpha) else 0
        x$data <- x$data / sum(abs(x$data)) # Computes frequencies
        conf_center <- x$data # Center of confidence intervals
        cbind.data.frame(ci = ci, conf_center = conf_center, x)
      },
      alpha = 1 - level
    )
    data <- do.call(rbind.data.frame, data)

    if (EPPM) {
      # Build long table from threshold
      threshold <- independance(object, method = "EPPM")
      threshold_stacked <- utils::stack(as.data.frame(threshold))
      threshold <- cbind.data.frame(case = row_names, threshold_stacked)
      colnames(threshold) <- c("case", "EPPM", "type")
      # Preserves the original ordering of the columns
      threshold$type <- factor(threshold$type, levels = unique(threshold$type))

      # Join data and threshold
      data <- merge(data, threshold, by = c("case", "type"), all = TRUE)
      data$data <- data$data - data$EPPM
      data_stacked <- utils::stack(data[, !(names(data) %in% c("case", "type", "conf_center", "ci"))])
      data <- cbind.data.frame(data$case, data$type, data$conf_center, data$ci, data_stacked)
      colnames(data) <- c("case", "type", "conf_center", "ci", "data", "threshold")
      # Remove CI on EPPM values
      data$ci <- ifelse(data$threshold == "EPPM", 0, data$ci)
    }
    if (center) {
      k <- nrow(data)
      z <- c(rep(1, k), rep(-1, k)) / 2
      data <- rbind.data.frame(data, data)
      data$data <- data$data * z
      data$ci <- data$ci * z
      data$conf_center <- data$conf_center * z
    }

    # Rename axis
    names(data)[names(data) == "data"] <- "frequency"

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
#' @aliases plotBar,AbundanceMatrix-method
setMethod(
  f = "plotBar",
  signature = signature(object = "AbundanceMatrix"),
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
#' @aliases plotMatrix,AbundanceMatrix-method
setMethod(
  f = "plotMatrix",
  signature = signature(object = "AbundanceMatrix"),
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
#' @aliases plotRank,AbundanceMatrix-method
setMethod(
  f = "plotRank",
  signature = signature(object = "AbundanceMatrix"),
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
    freq <- methods::as(object, "AbundanceMatrix")
    plot_spot(freq, threshold = threshold)
  }
)
#' @export
#' @rdname deprecated
#' @aliases plotSpot,AbundanceMatrix-method
setMethod(
  f = "plotSpot",
  signature = signature(object = "AbundanceMatrix"),
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
    .Deprecated(msg = "seriate is deprecated. Use seriate_reciprocal or seriate_correspondence instead.")
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
    .Deprecated(msg = "seriate is deprecated. Use seriate_reciprocal or seriate_correspondence instead.")
    seriation(object * 1, method = method, margin = margin, stop = stop, ...)
  }
)
#' @export
#' @rdname deprecated
setMethod(
  f = "seriate",
  signature = signature(object = "CountMatrix", subset = "BootCA"),
  definition = function(object, subset, margin = c(1, 2), ...) {
    .Deprecated(msg = "seriate is deprecated. Use seriate_correspondence instead.")
    seriate_correspondence(object, subset, margin = margin, ...)
  }
)

# ==================================================================== Diversity

#' @export
#' @rdname deprecated
setMethod(
  f = "diversity",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("berger", "brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        simplify = FALSE, ...) {
    .Deprecated(new = "index_heterogeneity")
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    H <- lapply(
      X = method,
      FUN = function(x, data) {
        index <- switch_heterogeneity(x)
        apply(X = object, MARGIN = 1, FUN = index)
      },
      data = object)
    names(H) <- method
    if (simplify)
      H <- simplify2array(H, higher = FALSE)
    return(H)
  }
)
#' @export
#' @rdname deprecated
setMethod(
  f = "evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("brillouin", "mcintosh",
                                           "shannon", "simpson"),
                        simplify = FALSE, ...) {
    .Deprecated(new = "index_evenness")
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- lapply(X = method, FUN = function(x, data) {
      index <- switch_evenness(x)
      apply(X = object, MARGIN = 1, FUN = index)
    }, data = object)
    names(E) <- method
    if (simplify)
      E <- simplify2array(E, higher = FALSE)
    return(E)
  }
)
#' @export
#' @rdname deprecated
setMethod(
  f = "richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, method = c("ace", "chao1",
                                           "margalef", "menhinick", "none"),
                        unbiased = FALSE, improved = FALSE, k = 10,
                        simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- lapply(
      X = method,
      FUN = function(x, object, unbiased, improved, k) {
        index <- switch (
          x,
          ace = richnessACE,
          chao1 = richnessChao1,
          margalef = richnessMargalef,
          menhinick = richnessMenhinick,
          none = function(x, ...) { sum(x > 0) },
          stop(sprintf("There is no such method: %s.", x), call. = FALSE)
        )
        apply(X = object, MARGIN = 1, FUN = index, unbiased = unbiased,
              improved = improved, k = k)
      },
      object, unbiased, improved, k
    )
    names(E) <- method
    if (simplify)
      E <- simplify2array(E, higher = FALSE)
    return(E)
  }
)
#' @export
#' @rdname deprecated
setMethod(
  f = "richness",
  signature = signature(object = "IncidenceMatrix"),
  definition = function(object, method = c("chao2", "ice"),
                        unbiased = FALSE, improved = FALSE, k = 10,
                        simplify = FALSE) {
    # Validation
    method <- match.arg(method, several.ok = TRUE)
    E <- lapply(
      X = method,
      FUN = function(x, object, unbiased, improved, k) {
        index <- switch (
          x,
          ice = richnessICE,
          chao2 = richnessChao2,
          stop(sprintf("There is no such method: %s.", method), call. = FALSE)
        )
        index(object, unbiased = unbiased, improved = improved, k = k)
      },
      object, unbiased, improved, k
    )
    names(E) <- method
    if (simplify)
      E <- simplify2array(E, higher = FALSE)
    return(E)
  }
)
