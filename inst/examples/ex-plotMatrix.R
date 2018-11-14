# Abundance data
## Coerce dataset to abundance matrix
## Data from Desachy 2004
count <- as(compiegne, "CountMatrix")

# Plot matrix diagram...
## ...without threshod (i.e. heatmap)
plotMatrix(count)

## ...with PVI as threshold (i.e. Bruno Desachy's matrigraphe)
plotMatrix(count, PVI = TRUE) +
  ggplot2::scale_fill_gradient2(midpoint = 1)

# Presence/absence data
bin <- IncidenceMatrix(data = sample(0:1, size = 100, replace = TRUE),
                       nrow = 10, ncol = 10)

# Plot matrix diagram
plotMatrix(bin) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))
