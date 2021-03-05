## Plot spot diagram of count data...
data("mississippi", package = "folio")
counts <- as_count(mississippi)

### ...without threshod
plot_spot(counts)
### ...with the  column means as threshold
plot_spot(counts, threshold = mean)
### ...with the column medians as threshold
plot_spot(counts, threshold = median)
