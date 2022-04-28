# PLOT DIVERSITY
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @method autoplot DiversityIndex
autoplot.DiversityIndex <- function(object, ...) {
  ## Prepare data
  count <- cbind.data.frame(
    label = names(object[["values"]]),
    x = object[["size"]],
    y = object[["values"]]
  )

  ## Simulated assemblages
  gg_sim <- NULL
  if (length(object[["simulation"]]) != 0) {
    # Build a long table for ggplot2
    refined <- object[["simulation"]]
    sim_stacked <- arkhe::as_long(refined[, -c(1)], factor = TRUE)
    sim <- cbind.data.frame(
      size = refined[, 1],
      sim_stacked,
      type = ifelse(sim_stacked[["column"]] == "mean", "mean", "conf. int.")
    )
    gg_sim <- ggplot2::geom_path(
      mapping = ggplot2::aes(
        x = .data$size,
        y = .data$value,
        colour = .data$type,
        group = .data$column
      ),
      data = sim,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  }

  y_lab <- switch (
    class(object),
    HeterogeneityIndex = "Heterogeneity",
    EvennessIndex = "Evenness",
    RichnessIndex = "Richness",
    "Diversity"
  )

  ## ggplot
  ggplot2::ggplot(data = count) +
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label) +
    ggplot2::geom_point() +
    gg_sim +
    ggplot2::scale_x_log10(name = "Sample size") +
    ggplot2::scale_y_continuous(
      name = sprintf("%s (%s)", y_lab, object[["method"]])
    )
}

#' @export
#' @rdname plot_diversity
#' @aliases autoplot,DiversityIndex-method
setMethod("autoplot", "DiversityIndex", autoplot.DiversityIndex)

#' @export
#' @method plot DiversityIndex
plot.DiversityIndex <- function(x, ...) {
  gg <- autoplot(object = x) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_diversity
#' @aliases plot,DiversityIndex,missing-method
setMethod("plot", c(x = "DiversityIndex", y = "missing"), plot.DiversityIndex)
