# Plot spot diagram...

## ...of count data...
count <- as(mississippi, "CountMatrix")
### ...without threshod
plotSpot(count)
### ...with the  column means as threshold
plotSpot(count, threshold = mean)
### ...with the column medians as threshold
plotSpot(count, threshold = median)

## ...of a similarity matrix
sim <- similarity(count, "brainerd")
plotSpot(sim)

## ...of a co-occurence matrix
occ <- as(mississippi, "OccurrenceMatrix")
plotSpot(occ)
