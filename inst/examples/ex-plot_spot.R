## Plot spot diagram...

## ...of count data...
counts <- as_count(mississippi)
### ...without threshod
plot_spot(counts)
### ...with the  column means as threshold
plot_spot(counts, threshold = mean)
### ...with the column medians as threshold
plot_spot(counts, threshold = median)

## ...of a similarity matrix
sim <- similarity(counts, method = "brainerd")
plot_spot(sim)

## ...of a co-occurrence matrix
occ <- as_occurrence(mississippi)
plot_spot(occ)
