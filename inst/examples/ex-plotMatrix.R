# Coerce dataset to abundance matrix
# Data from Desachy 2004
count <- as(compiegne, "CountMatrix")
freq <- as(boves, "FrequencyMatrix")

# Plot matrix diagram...
## ...without threshod (i.e. heatmap)
plotMatrix(count)
plotMatrix(freq)

## ...with PVI as threshold (i.e. Bruno Desachy's matrigraphe)
plotMatrix(count, PVI = TRUE)
plotMatrix(count, PVI = TRUE, center = TRUE)

# Presence/absence data
bin <- IncidenceMatrix(data = sample(0:1, size = 100, replace = TRUE),
                       nrow = 10, ncol = 10)

# Plot matrix diagram
plotMatrix(bin) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))
