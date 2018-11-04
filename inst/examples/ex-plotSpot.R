# Coerce dataset to count matrix
count <- as(compiegne, "CountMatrix")

# Plot spot diagram...
## ...without threshod
plotSpot(count)

## ...with column means as threshold
plotSpot(count, threshold = "mean")


# Coerce dataset to frequency matrix
freq <- as(boves, "FrequencyMatrix")

# Plot spot diagram with column medians as threshold
plotSpot(freq, threshold = "median")
