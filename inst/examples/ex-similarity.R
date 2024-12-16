## Data from Huntley 2004, 2008
data("pueblo")

## Brainerd-Robinson measure
(C <- similarity(pueblo, "brainerd"))
plot_spot(C)

## Data from Magurran 1988, p. 166
data("aves")

## Jaccard measure (presence/absence data)
similarity(aves, "jaccard") # 0.46

# Bray and Curtis modified version of the Sorenson index (count data)
(sim <- similarity(aves, "bray")) # 0.44

# Bray and Curtis dissimilarity
1 - sim
