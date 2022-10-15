data("compiegne", package = "folio")

## Seriograph
seriograph(compiegne) +
  ggplot2::theme(legend.position = "bottom") +
  khroma::scale_fill_highcontrast()

## Compute EPPM
counts_eppm <- eppm(compiegne)
plot_heatmap(counts_eppm) +
  khroma::scale_fill_YlOrBr(name = "EPPM")
