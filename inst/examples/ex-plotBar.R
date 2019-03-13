\donttest{

# Plot Ford diagram...
count <- as(compiegne, "CountMatrix") # Data from Desachy 2004
## ...without threshold
plotBar(count)
## ...with EPPM (i.e. Bruno Desachy's sériographe)
plotBar(count, EPPM = TRUE)

# Plot Bertin diagram...
freq <- as(boves, "FrequencyMatrix") # Data from Desachy 2004
## ...without EPPM...
plotBar(freq, center = FALSE, horizontal = TRUE)
## ...and confidence intervals
plotBar(freq, level = 0.05, center = FALSE, horizontal = FALSE)
## ...with EPPM (i.e. Bruno Desachy's sériographe)
plotBar(freq, EPPM = TRUE, center = FALSE, horizontal = TRUE)
}
