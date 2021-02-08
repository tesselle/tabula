## Abundance data
## Coerce dataset to a count matrix (data from Desachy 2004)
counts <- as_count(compiegne)

## Compute EPPM
counts_eppm <- eppm(counts)

## Compute PVI
counts_pvi <- pvi(counts)
plot_heatmap(counts_eppm)
