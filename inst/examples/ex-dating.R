# Coerce dataset to abundance (count) matrix
zuni <- as(zuni, "CountMatrix")

# Assume that some assemblages are reliably dated (this is NOT a real example).
# The name of the vector elements must match the names of the assemblages.
setDates(zuni) <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

# Model the event and accumulation date for each assemblage.
(model <- dateEvent(zuni, cutoff = 90))

\donttest{
# Plot results for the first five assemblages
# Plot event date distribution
plotDate(model, type = "event", select = 1:5) +
  ggplot2::theme_bw()
# Plot accumulation date distribution (activity plot)
plotDate(model, type = "acc", select = 1:5) +
  ggplot2::theme_bw()

# Plot both distributions
plotDate(model, select = "LZ1105") +
  ggplot2::theme_bw()
# Tempo plot
plotDate(model, type = "accumulation", tempo = TRUE) +
  ggplot2::theme_bw()

# Check model variability
## Jackknife fabrics
refined_jack <- refine(model, method = "jackknife", n = 1000)
head(refined_jack)

## Bootstrap of assemblages
refined_boot <- refine(model, method = "bootstrap", n = 1000)
head(refined_boot)
}
