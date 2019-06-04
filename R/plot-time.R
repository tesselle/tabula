# PLOT LINE
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plotDate-method
#' @aliases plotTime,CountMatrix-method
setMethod(
  f = "plotTime",
  signature = signature(object = "CountMatrix"),
  definition = function(object, highlight = NULL, level = 0.95,
                        roll = FALSE, window = 5, facet = TRUE, ...) {
    # Validation
    if (!is.null(highlight)) {
      highlight <- match.arg(highlight, choices = c("FIT"), several.ok = FALSE)
    } else {
      highlight <- "none"
    }
    if (highlight == "FIT") facet <- TRUE # Override default
    alpha <- 1 - level

    # Prepare data
    gg_roll <- NULL
    # Get row names and coerce to factor (preserve original ordering)
    row_names <- rownames(object) %>% factor(levels = unique(.))
    # Get number of cases
    n <- length(row_names)
    # Get time coordinates
    time <- getDates(object)[, 1, drop = TRUE]
    if (isEmpty(time))
        stop("Time coordinates are missing!", call. = FALSE)

    data <- object %>%
      as.data.frame() %>%
      dplyr::mutate(case = row_names,
                    time = time) %>%
      tidyr::gather(key = "type", value = "frequency", -.data$case, -.data$time,
                    factor_key = TRUE) %>%
      dplyr::filter(.data$frequency > 0) # Remove zeros in case of log scale

    if (highlight == "FIT") {
      signature <- testFIT(object, time, roll = FALSE)[[1L]] %>%
        as.data.frame() %>%
        dplyr::transmute(
          type = factor(rownames(.), levels = unique(rownames(.))),
          signature = ifelse(.data$p.value <= alpha, "selection", "neutral")
        )

      data %<>% dplyr::left_join(y = signature, by = c("type"))

      if (roll) {
        k <- (window - 1) / 2
        fit <- object %>%
          testFIT(time, roll = roll, window = window) %>%
          lapply(FUN = function(x) {
            x %>%
              as.data.frame() %>%
              dplyr::mutate(
                type = factor(rownames(.), levels = unique(rownames(.)))
              )
          }) %>%
          dplyr::bind_rows(.id = "w") %>%
          dplyr::transmute(
            type = .data$type,
            sub_signature = ifelse(.data$p.value <= alpha,
                                   "selection", "neutral"),
            time = time[as.integer(.data$w)]
          )

        data %<>% dplyr::left_join(y = fit, by = c("type", "time")) %>%
          dplyr::arrange(.data$type, .data$time) %>%
          dplyr::group_by(.data$type) %>%
          dplyr::mutate(
            sub_signature = ifelse(
              vapply(
                X = seq_along(.data$sub_signature),
                FUN = function(x, var, k) {
                  max <- length(var)
                  lower <- x - k
                  lower[lower < 1] <- 1
                  upper <- x + k
                  upper[upper > max] <- max
                  any(var[lower:upper] == "selection")
                },
                FUN.VALUE = logical(1),
                var = .data$sub_signature, k = k
              ),
              "selection", "neutral")
          ) %>%
          dplyr::ungroup()

        gg_roll <- data %>%
          filter(.data$sub_signature == "selection") %>%
          geom_line(mapping = aes_string(group = "type"), size = 5,
                    colour = "grey80", lineend = "round")
      }
    }

    data %<>% dplyr::arrange(.data$type, .data$time)

    # ggplot
    if (facet) {
      facet <- facet_wrap(. ~ type, scales = "free_y")
      colour <- if (highlight == "FIT") "signature" else NULL
    } else {
      colour <- "type"
      facet <- NULL
    }

    ggplot(data = data,
           mapping = aes_string(x = "time", y = "frequency", colour = colour)) +
      gg_roll + geom_point() + geom_line() + facet
  }
)
