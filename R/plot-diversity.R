# PLOT DIVERSITY
#' @include AllGenerics.R AllClasses.R plot-prepare.R
NULL

#' @export
#' @rdname richness
#' @aliases plot_richness,CountMatrix-method
setMethod(
  f = "plot_richness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, prob = NULL, level = 0.80, n = 1000) {

    fun <- function(x) { sum(x > 0) }
    plot_diversity(object, diversity = fun, prob = prob, level = level, n = n) +
      labs(x = "Sample size", y = "Number of observed taxa",
           colour = "Simulation")
  }
)

#' @export
#' @rdname diversity
#' @aliases plot_evenness,CountMatrix-method
setMethod(
  f = "plot_evenness",
  signature = signature(object = "CountMatrix"),
  definition = function(object, prob = NULL, level = 0.80, n = 1000) {

    fun <- function(x) { evennessShannon(x, zero.rm = FALSE) }
    plot_diversity(object, diversity = fun, prob = prob, level = level, n = n) +
      labs(x = "Sample size", y = "Evenness", colour = "Simulation")
  }
)

plot_diversity <- function(object, diversity,
                           prob = NULL, level = 0.80, n = 1000) {
  # Prepare data
  conf_level <- paste0(round(level * 100), "% conf. int.")
  count <- cbind.data.frame(
    label = rownames(object),
    x = rowSums(object),
    y = apply(X = object, MARGIN = 1, FUN = diversity)
  )

  refined <- refine_diversity(x = object, method = diversity,
                              prob = prob, level = level, n = n)
  # Build a long table for ggplot2
  data_stacked <- utils::stack(as.data.frame(refined), select = -c(1))
  data <- cbind.data.frame(
    size = refined[, 1],
    data_stacked,
    type = ifelse(data_stacked[["ind"]] == "mean", "mean", conf_level)
  )

  # ggplot
  ggplot(data = count, mapping = aes(x = .data$x, y = .data$y,
                                     label = .data$label)) +
    geom_path(mapping = aes(x = .data$size, y = .data$values,
                            colour = .data$type, group = .data$ind),
              data = data, inherit.aes = FALSE) +
    geom_point() +
    scale_x_log10()
}
