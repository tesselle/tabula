## Data from Desachy 2004
data("compiegne", package = "folio")

## Matrigraph
matrigraph(compiegne)
matrigraph(compiegne, reverse = TRUE)

## Compute PVI
counts_pvi <- pvi(compiegne)
plot_heatmap(counts_pvi, col = khroma::color("iridescent")(12))
