\dontrun{

# Coerce dataset to abundance matrix
# Data from Desachy 2004
count <- as(compiegne, "CountMatrix")
freq <- as(boves, "FrequencyMatrix")

# Plot Ford diagram...
## ...without threshold
plotBar(count)
plotBar(freq)

## ...with EPPM (i.e. Bruno Desachy's sériographe)
plotBar(count, EPPM = TRUE)
plotBar(freq, EPPM = TRUE)

# Plot Bertin diagram...
## ...without EPPM...
plotBar(count, center = FALSE, horizontal = TRUE)
## ...and confidence intervals
plotBar(freq, level = FALSE, center = FALSE, horizontal = FALSE)

## ...with EPPM (i.e. Bruno Desachy's sériographe)
plotBar(count, EPPM = TRUE, center = FALSE, horizontal = TRUE)
}
