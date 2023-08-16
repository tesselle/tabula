## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Plot raw data
plot_heatmap(cantabria)

## Plot conditional proportions
plot_heatmap(cantabria, freq = TRUE, margin = 1)
plot_heatmap(cantabria, freq = TRUE, margin = 2)
