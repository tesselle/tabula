## Abundance data (data from Desachy 2004)
data("compiegne", package = "folio")
## Coerce dataset to absolute frequencies
counts <- as_count(compiegne)
## Coerce dataset to relative frequencies
freq <- as_composition(compiegne)

## Plot matrix diagram...
plot(counts)
plot(freq)

## Presence/absence data
inc <- sample(0:1, size = 100, replace = TRUE)
bin <- IncidenceMatrix(data = inc, nrow = 10, ncol = 10)

autoplot(bin) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))
