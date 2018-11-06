# Coerce dataset to abundance matrix
# Data from Desachy 2004
count <- as(compiegne, "CountMatrix")
freq <- as(boves, "FrequencyMatrix")

# Plot spot diagram...
## ...without threshod
plotSpot(count)

## ...with column means as threshold
plotSpot(count, threshold = "mean")

# Plot spot diagram with column medians as threshold
plotSpot(freq, threshold = "median")
