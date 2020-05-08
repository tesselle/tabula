## Coerce dataset to a count matrix
counts <- as_count(compiegne)

## Computes correspondence analysis
ca <- run_ca(counts)
plot_ca(ca, map = c("rows", "columns"))

