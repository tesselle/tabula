## Data from Lipo et al. 2015
data("mississippi", package = "folio")

## Plot raw data
plot_heatmap(mississippi)

## Change colors
plot_heatmap(mississippi, color = color("iridescent"))

## Plot conditional proportions
plot_heatmap(mississippi, freq = TRUE, margin = 1)
plot_heatmap(mississippi, freq = TRUE, margin = 2)
