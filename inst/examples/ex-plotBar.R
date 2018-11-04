# Coerce dataset to count matrix
count <- as(compiegne, "CountMatrix")
freq <- as(boves, "FrequencyMatrix")

# Plot Ford diagram...
## ...without threshold
plotBar(count)
plotBar(freq)

## ...with EPPM as threshold (i.e. Bruno Desachy's sériographe)
plotBar(count, EPPM = TRUE)
plotBar(freq, EPPM = TRUE)

# Plot Bertin diagram...
## ...without threshold
plotBar(count, center = FALSE, horizontal = TRUE)
plotBar(freq, center = FALSE)

## ...with EPPM as threshold (i.e. Bruno Desachy's sériographe)
plotBar(count, EPPM = TRUE, center = FALSE, horizontal = TRUE)
