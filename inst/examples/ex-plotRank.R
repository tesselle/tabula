# Coerce dataset to count matrix
count <- as(compiegne, "CountMatrix")

# Plot rank diagram
plotRank(count)
plotRank(count, log = "xy")

# Coerce dataset to frequency matrix
freq <- as(boves, "FrequencyMatrix")

# Plot rank diagram
plotRank(freq, facet = FALSE)
plotRank(freq, facet = FALSE, log = "xy")
