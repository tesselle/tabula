\donttest{
# Coerce dataset to abundance (count) matrix
zuni <- as(zuni, "CountMatrix")

# Assume that some assemblages are reliably dated (this is NOT a real example).
# The names of the vector entries must match the names of the assemblages.
dates <- list(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

# Model the event and accumulation date for each assemblage.
(model <- dateEvent(zuni, dates, cutoff = 90))

# Plot results for the first five assemblages
# Plot event date distribution
plotDate(model, type = "event", select = 1:5) +
  ggplot2::theme_bw()
# Plot accumulation date distribution
plotDate(model, type = "acc", select = 1:5) +
  ggplot2::theme_bw()
# Plot both distributions
plotDate(model, select = "LZ1105") +
  ggplot2::theme_bw()

# Check model variability
checked <- dateEvent(zuni, dates, cutoff = 90,
                     jackknife = TRUE, bootstrap = TRUE, n = 1000)

# Extract results for the first 5 assemblages
## Modeled event dates
checked["rows", 1:5]
## Jackknife fabrics
checked["jackknife", 1:5]
## Bootstrap of assemblages
checked["bootstrap", 1:5]
}
