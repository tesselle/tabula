## Sample observations from a multinomial distribution
x <- sample(1:100, 50, TRUE)
resample(x, do = median, n = 100)

## Estimate the 25th, 50th and 95th percentiles
quant <- function(x) { quantile(x, probs = c(0.25, 0.50, 0.75)) }
resample(x, n = 100, do = median, f = quant)
