## Plot spot diagram...

## ...of count data...
count <- as(mississippi, "CountMatrix")
### ...without threshod
plot_spot(count)
### ...with the  column means as threshold
plot_spot(count, threshold = mean)
### ...with the column medians as threshold
plot_spot(count, threshold = median)

## ...of a similarity matrix
sim <- similarity(count, "brainerd")
plot_spot(sim)

## ...of a co-occurrence matrix
occ <- as(mississippi, "OccurrenceMatrix")
plot_spot(occ)
