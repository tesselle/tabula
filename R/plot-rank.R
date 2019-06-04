# PLOT RANK
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plotLine
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
#' @rdname plotLine
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
