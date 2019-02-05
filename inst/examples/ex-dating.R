# Coerce dataset to abundance (count) matrix
zuni <- as(zuni, "CountMatrix")

# Assume that some assemblages are reliably dated (this is NOT a real example).
# The names of the vector entries must match the names of the assemblages.
dates <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

# Model the event and accumulation date for each assemblage.
(model <- dateEvent(zuni, dates, axes = 10))

# Print the modeled event and accumulation dates
head(model[["rows"]])
head(model[["accumulation"]])

# Plot results for the second assemblage
# Plot event date distribution
plotDate(zuni, model, type = "event", select = 2) +
  ggplot2::theme_bw()
# Plot accumulation date distribution
plotDate(zuni, model, type = "acc", select = 2) +
  ggplot2::theme_bw()
# Plot both distributions
plotDate(zuni, model, select = 2) +
  ggplot2::theme_bw()

# Plot estimated event date with confidence interval
plotBar(model, select = 1:20) +
  ggplot2::theme_bw()
