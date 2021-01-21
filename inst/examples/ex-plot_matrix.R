## Abundance data (data from Desachy 2004)
## Coerce dataset to absolute frequencies
counts <- as_count(compiegne)
## Coerce dataset to relative frequencies
freq <- as_abundance(compiegne)

## Plot matrix diagram...
## ...without threshod (i.e. heatmap)
plot_heatmap(counts)
plot_heatmap(freq)
## ...with PVI as threshold (i.e. Bruno Desachy's matrigraphe)
plot_heatmap(counts, PVI = TRUE) +
  ggplot2::scale_fill_gradient2(midpoint = 1)

## Presence/absence data
inc <- sample(0:1, size = 100, replace = TRUE)
bin <- IncidenceMatrix(data = inc, nrow = 10, ncol = 10)

plot_heatmap(bin) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))
