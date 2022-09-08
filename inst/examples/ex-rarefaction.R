data("cantabria")

## Replicate fig. 3 from Baxter 2011
rare <- rarefaction(cantabria, sample = 23, method = "baxter")
plot(rare)
