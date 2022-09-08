data("cantabria")

## Compute EPPM
counts_eppm <- eppm(cantabria)
plot_heatmap(counts_eppm) +
  khroma::scale_fill_YlOrBr(name = "EPPM")

## Compute PVI
counts_pvi <- pvi(cantabria)
plot_heatmap(counts_pvi) +
  khroma::scale_fill_BuRd(name = "PVI", midpoint = 1)
