# Coerce dataset to abundance matrix
# Data from Desachy 2004
count <- as(compiegne, "CountMatrix")

# Plot spot diagram...
## ...without threshod
plotSpot(count)

## ...with the  column means as threshold
plotSpot(count, threshold = mean)

## ...with the column medians as threshold
plotSpot(count, threshold = median)
