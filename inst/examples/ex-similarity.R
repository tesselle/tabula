## Data from Huntley 2004, 2008
data("pueblo")

## Brainerd-Robinson measure
(C <- similarity(pueblo, "brainerd"))
plot_spot(C)

## Data from Magurran 1988, p. 166
birds <- matrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9),
  nrow = 2, byrow = TRUE
)
rownames(birds) <- c("unmanaged", "managed")

## Jaccard measure (presence/absence data)
similarity(birds, "jaccard") # 0.46

## Sorenson measure (presence/absence data)
similarity(birds, "sorenson") # 0.63

# Jaccard measure (Bray's formula ; count data)
similarity(birds, "bray") # 0.44

# Morisita-Horn measure (count data)
similarity(birds, "morisita") # 0.81
