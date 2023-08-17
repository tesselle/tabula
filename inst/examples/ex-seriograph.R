## Data from Desachy 2004
data("compiegne", package = "folio")

## Seriograph
seriograph(compiegne)
seriograph(compiegne, weights = TRUE)

## Compute EPPM
counts_eppm <- eppm(compiegne)
plot_heatmap(counts_eppm, col = khroma::color("YlOrBr")(12))
