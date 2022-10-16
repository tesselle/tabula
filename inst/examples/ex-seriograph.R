data("compiegne", package = "folio")

## Seriograph
seriograph(compiegne)

## Compute EPPM
counts_eppm <- eppm(compiegne)
plot_heatmap(counts_eppm) +
  khroma::scale_fill_YlOrBr(name = "EPPM")
