# PREPARE DATASETS FOR PLOTTING METHODS
# All these functions must return a data.frame

# Prepare data for Bertin plot
prepare_bertin <- function(object, threshold = NULL, scale = NULL) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = unique(row_names))

  # Build a long table for ggplot2
  data_stacked <- utils::stack(as.data.frame(object))
  data <- cbind.data.frame(case = row_names, data_stacked)
  colnames(data) <- c("case", "frequency", "type")
  # Preserves the original ordering of the columns
  data$type <- factor(data$type, levels = unique(data$type))

  # Scale variables
  if (is.function(scale)) {
    data <- by(
      data,
      INDICES = data$type,
      FUN = function(x, fun) {
        x$frequency = fun(x$frequency)
        x
      },
      fun = scale
    )
    data <- do.call(rbind.data.frame, data)
  }

  # Compute threshold, if any
  if (is.function(threshold)) {
    data <- by(
      data,
      INDICES = data$type,
      FUN = function(x, fun) {
        x$thresh = fun(x$frequency)
        x
      },
      fun = threshold
    )
    data <- do.call(rbind.data.frame, data)
    threshold <- ifelse(data$frequency > data$thresh, "above", "below")
    data <- cbind.data.frame(threshold, data)
  }

  return(data)
}

# Prepare data for Ford plot
prepare_ford <- function(object, EPPM = FALSE) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = rev(unique(row_names)))

  # Build a long table for ggplot2
  data <- object / rowSums(object)
  data_stacked <- utils::stack(as.data.frame(data))
  data <- cbind.data.frame(case = row_names, data_stacked)
  colnames(data) <- c("case", "data", "type")
  # Preserves the original ordering of the columns
  data$type <- factor(data$type, levels = unique(data$type))

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
    data_stacked <- utils::stack(data[, !(names(data) %in% c("case", "type"))])
    data <- cbind.data.frame(data$case, data$type, data_stacked)
    colnames(data) <- c("case", "type", "data", "threshold")
  }

  k <- nrow(data)
  z <- c(rep(1, k), rep(-1, k)) / 2
  data <- rbind.data.frame(data, data)
  data$data <- data$data * z

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
    data <- independance(object, method = "PVI")
    data_stacked <- utils::stack(as.data.frame(data))
    data <- cbind.data.frame(row_names, data_stacked)
    colnames(data) <- c("case", "PVI", "type")
    # Preserves the original ordering of the columns
    data$type <- factor(data$type, levels = unique(data$type))
  } else {
    # Build long table from data
    data <- if (frequency) object / rowSums(object) else object
    data_stacked <- utils::stack(as.data.frame(data))
    data <- cbind.data.frame(row_names, data_stacked)
    colnames(data) <- c("case", "Frequency", "type")
    # Preserves the original ordering of the columns
    data$type <- factor(data$type, levels = unique(data$type))
  }

  # Tile centers
  data <- cbind.data.frame(
    data,
    x = as.numeric(data$type),
    y = as.numeric(data$case)
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

  # Build long table from data
  data_stacked <- utils::stack(as.data.frame(object))
  data <- cbind.data.frame(row_names, data_stacked)
  colnames(data) <- c("case", "frequency", "type")
  # Preserves the original ordering of the columns
  data$type <- factor(data$type, levels = unique(data$type))

  # Remove zeros in case of log scale
  data <- data[data$frequency > 0, ]

  data <- by(
    data,
    INDICES = data$case,
    FUN = function(x) {
      data <- x[order(x$frequency, decreasing = TRUE), ]
      data <- cbind.data.frame(rank = seq_len(nrow(data)), data)
      data
    }
  )
  data <- do.call(rbind.data.frame, data)
  return(data)
}

prepare_spot <- function(object, threshold = NULL, diag = TRUE) {
  # Get row names and coerce to factor (preserve original ordering)
  row_names <- rownames(object)
  row_names <- factor(x = row_names, levels = unique(row_names))

  # Build a long table from data
  data <- object #* 0.8
  data_stacked <- utils::stack(as.data.frame(data))
  data <- cbind.data.frame(row_names, data_stacked)
  colnames(data) <- c("case", "value", "type")
  # Preserves the original ordering of the columns
  data$type <- factor(data$type, levels = unique(data$type))

  if (!diag) {
    data <- data[data$type != data$case, ]
  }
  if (nrow(object) == ncol(object)) {
    max_value <- unique(diag(object))
    if (max_value == 0) max_value <- max(data$value)
    data <- cbind.data.frame(max = max_value, data)
  }
  if (is.function(threshold)) {
    data <- by(
      data,
      INDICES = data$type,
      FUN = function(x, fun) {
        data <- cbind.data.frame(thresh = fun(x$value), x)
        data
      },
      fun = threshold
    )
    data <- do.call(rbind.data.frame, data)
    threshold <- ifelse(data$value > data$thresh, "above", "below")
    data <- cbind.data.frame(threshold = threshold, data)
  }
  return(data)
}
