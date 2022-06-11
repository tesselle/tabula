## Abundance data
## Coerce dataset to a count matrix (data from Desachy 2004)
data("compiegne", package = "folio")

## Compute EPPM
counts_eppm <- eppm(compiegne)

## Compute PVI
counts_pvi <- pvi(compiegne)
plot_heatmap(counts_pvi)
