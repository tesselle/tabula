## Data from Huntley 2004, 2008
data("pueblo")

## Brainerd-Robinson measure
(C <- similarity(pueblo, "brainerd"))
plot_spot(C)

## Data from Magurran 1988, p. 166
data("birds")

## Jaccard measure (presence/absence data)
similarity(birds, "jaccard") # 0.46

## Sorenson measure (presence/absence data)
similarity(birds, "sorenson") # 0.63

# Jaccard measure (Bray's formula ; count data)
similarity(birds, "bray") # 0.44

# Morisita-Horn measure (count data)
similarity(birds, "morisita") # 0.81
