## Abundance data
## Coerce dataset to a count matrix (data from Desachy 2004)
data("compiegne", package = "folio")

## Compute EPPM
counts_eppm <- eppm(compiegne)
plot_heatmap(counts_eppm) +
  khroma::scale_fill_YlOrBr(name = "EPPM")

## Compute PVI
counts_pvi <- pvi(compiegne)
plot_heatmap(counts_pvi) +
  khroma::scale_fill_BuRd(name = "PVI", midpoint = 1)
