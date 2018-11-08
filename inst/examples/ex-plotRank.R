# Coerce dataset to abundance matrix
# Data from Desachy 2004
count <- as(compiegne, "CountMatrix")
freq <- as(boves, "FrequencyMatrix")

# Plot rank vs abundance
plotRank(count)
plotRank(count, log = "xy")

# Plot rank vs abundance
plotRank(freq, facet = FALSE)
plotRank(freq, facet = FALSE, log = "xy")
