## Abundance data
## Coerce dataset to a count matrix (data from Desachy 2004)
count <- as(compiegne, "CountMatrix")

## Plot matrix diagram...
## ...without threshod (i.e. heatmap)
plot_heatmap(count)
## ...with PVI as threshold (i.e. Bruno Desachy's matrigraphe)
plot_heatmap(count, PVI = TRUE) +
  ggplot2::scale_fill_gradient2(midpoint = 1)

## Presence/absence data
bin <- IncidenceMatrix(data = sample(0:1, size = 100, replace = TRUE),
                       nrow = 10, ncol = 10)
plot_heatmap(bin) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))
