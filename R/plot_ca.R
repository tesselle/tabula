# PLOT CORRESPONDENCE ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname ca
#' @aliases plot_ca,CA-method
setMethod(
  f = "plot_ca",
  signature = signature(object = "CA"),
  definition = function(object, axes = c(1, 2),
                        map = c("rows", "columns"), ...) {
    # Prepare data
    data <- prepare_ca(object, axes = axes, map = map, select = NULL)

    # ggplot
    ggplot2::ggplot(
      data = data,
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$legend,
                   shape = .data$legend)
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::coord_fixed() +
      ggplot2::labs(x = print_var(object, axes[[1L]]),
                    y = print_var(object, axes[[2L]]))
  }
)

#' @export
#' @rdname ca
#' @aliases plot_ca,BootCA-method
setMethod(
  f = "plot_ca",
  signature = signature(object = "BootCA"),
  definition = function(object, axes = c(1, 2), map = c("rows", "columns"),
                        keep = NULL, ...) {
    # Prepare data
    data <- prepare_bootca(object, axes = axes, map = map, keep = keep)

    # ggplot
    ggplot2::ggplot(
      data = data,
      ggplot2::aes(x = .data$x, y = .data$y,
                   group = .data$id, fill = .data$legend)
    ) +
      ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_polygon(alpha = data$alpha) +
      ggplot2::coord_fixed() +
      ggplot2::labs(x = print_var(object, axes[[1L]]),
                    y = print_var(object, axes[[2L]]))
  }
)

print_var <- function(object, axis = 1) {
  eig <- arkhe::get_eigenvalues(object)
  variance <- round(eig[axis, 2], 1)
  sprintf("CA%d (%g%%)", axis, variance)
}

prepare_ca <- function(object, axes = c(1, 2), map = c("rows", "columns"),
                       select = NULL) {
  # Validation
  map <- match.arg(map, several.ok = TRUE)

  row_data <- col_data <- NULL
  if ("rows" %in% map) {
    row_data <- data.frame(
      arkhe::get_coordinates(object, margin = 1, sup = TRUE)[, axes],
      legend = "rows",
      stringsAsFactors = FALSE
    )
  }
  if ("columns" %in% map) {
    col_data <- data.frame(
      arkhe::get_coordinates(object, margin = 2, sup = TRUE)[, axes],
      legend = "columns",
      stringsAsFactors = FALSE
    )
  }
  data <- rbind.data.frame(row_data, col_data)
  print(data)
  colnames(data) <- c("x", "y", "legend")
  # data <- rownames_to_column(data, id = "label")
  data
}

prepare_bootca <- function(object, axes = c(1, 2), map = c("rows", "columns"),
                           keep = c("rows", "columns")) {
  # Validation
  map <- match.arg(map, several.ok = TRUE)
  if (!is.null(keep))
    keep <- match.arg(keep, several.ok = TRUE)

  row_data <- col_data <- NULL
  if ("rows" %in% map) {
    row_data <- data.frame(
      object[["row_chull"]],
      legend = "rows",
      stringsAsFactors = FALSE
    )
    if ("rows" %in% keep) {
      row_data <- row_data[row_data$id %in% names(object[["keep"]][[1]]), ]
    }
  }
  if ("columns" %in% map) {
    col_data <- data.frame(
      object[["column_chull"]],
      legend = "columns",
      stringsAsFactors = FALSE
    )
    if ("columns" %in% keep) {
      col_data <- col_data[col_data$id %in% names(object[["keep"]][[2]]), ]
    }
  }
  data <- rbind.data.frame(row_data, col_data)
  data$alpha <- ifelse(data$legend == "rows", 0.05, 0.5)
  data
}
