# Coerce dataset to abundance matrix
# Data from Desachy 2004
count <- as(compiegne, "CountMatrix")

# Plot rank diagram
plotRank(count)
plotRank(count, log = "xy")

# Coerce dataset to frequency matrix
freq <- as(boves, "FrequencyMatrix")

# Plot rank diagram
plotRank(freq, facet = FALSE)
plotRank(freq, facet = FALSE, log = "xy")
