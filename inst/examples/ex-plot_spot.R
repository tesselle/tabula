data("cantabria")

## Plot spot diagram of count data...
### ...without threshod
plot_spot(cantabria)
### ...with the  column means as threshold
plot_spot(cantabria, threshold = mean)
### ...with the column medians as threshold
plot_spot(cantabria, threshold = median)
