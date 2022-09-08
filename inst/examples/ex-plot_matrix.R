data("cantabria")

## Plot matrix diagram...
plot_heatmap(cantabria)
plot_heatmap(cantabria, freq = TRUE)

## Presence/absence data
inc <- sample(0:1, size = 100, replace = TRUE)
bin <- matrix(data = as.logical(inc), nrow = 10, ncol = 10)

plot_heatmap(bin) +
  khroma::scale_fill_logical()
