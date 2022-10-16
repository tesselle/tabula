data("compiegne", package = "folio")

## Matrigraph
matrigraph(compiegne)
matrigraph(compiegne, reverse = TRUE)

## Compute EPPM
counts_pvi <- pvi(compiegne)
plot_heatmap(counts_pvi) +
  khroma::scale_fill_BuRd(name = "PVI", midpoint = 1)
