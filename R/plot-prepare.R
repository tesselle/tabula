# PREPARE DATASETS FOR PLOTTING METHODS
# All these functions must return a data.frame

# Prepare data for Bertin plot
prepare_bertin <- function(object, threshold = NULL, scale = NULL) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = unique(row_names))

  # Build a long table from data
  data <- object %>%
    as.data.frame() %>%
    dplyr::mutate(case = row_names) %>%
    tidyr::gather(key = "type", value = "frequency",
                  -.data$case, factor_key = TRUE)

  # Scale variables
  if (is.function(scale)) {
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

  return(data)
}

# Prepare data for Ford plot
prepare_ford <- function(object, EPPM = FALSE) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = rev(unique(row_names)))

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

  return(data)
}

# Prepare data for Heatmap plot
prepare_heatmap <- function(object, PVI = FALSE, frequency = TRUE) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = unique(row_names))

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
    data <- if (frequency) object / rowSums(object) else object
    data <- as.data.frame(data) %>%
      dplyr::mutate(case = row_names) %>%
      tidyr::gather(key = "type", value = "Frequency",
                    -.data$case, factor_key = TRUE)
  }

  # Tile centers
  data %<>% dplyr::mutate(
    x = as.numeric(.data$type),
    y = as.numeric(.data$case)
  )
  return(data)
}

# Prepare data for rank plot
prepare_rank <- function(object) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = unique(row_names))
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
  return(data)
}

prepare_spot <- function(object, threshold = NULL, diag = TRUE) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = unique(row_names))

  # Build a long table from data
  data <- object %>% #{ object * 0.8 } %>%
    as.data.frame() %>%
    dplyr::mutate(case = row_names) %>%
    tidyr::gather(key = "type", value = "value",
                  -.data$case, factor_key = TRUE)

  if (!diag) {
    data %<>% dplyr::filter(.data$type != .data$case)
  }
  if (isSquare(object)) {
    max_value <- unique(diag(object))
    data %<>% dplyr::mutate(max = max_value)
  }
  if (is.function(threshold)) {
    data %<>%
      dplyr::group_by(.data$type) %>%
      dplyr::mutate(thresh = threshold(.data$value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(threshold = dplyr::if_else(.data$value > .data$thresh,
                                               "above", "below"))
  }
  return(data)
}
