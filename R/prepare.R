# PREPARE DATASETS FOR PLOTTING METHODS
# All these functions must return a data.frame

# Prepare data for Bertin plot
.prepareBertin <- function(object, threshold = NULL, scale = NULL) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object) %>% factor(levels = rev(unique(.)))

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
.prepareFord <- function(object, EPPM = FALSE) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object) %>% factor(levels = rev(unique(.)))

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
