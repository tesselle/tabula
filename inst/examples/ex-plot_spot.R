## Plot spot diagram of count data...
data("mississippi", package = "folio")

### ...without threshod
plot_spot(mississippi)
### ...with the  column means as threshold
plot_spot(mississippi, threshold = mean)
### ...with the column medians as threshold
plot_spot(mississippi, threshold = median)
