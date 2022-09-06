## Ceramic data
data("compiegne", package = "folio")

## Plot matrix diagram...
plot_heatmap(compiegne)
plot_heatmap(compiegne, freq = TRUE)

## Presence/absence data
inc <- sample(0:1, size = 100, replace = TRUE)
bin <- matrix(data = as.logical(inc), nrow = 10, ncol = 10)

plot_heatmap(bin) +
  khroma::scale_fill_logical()
