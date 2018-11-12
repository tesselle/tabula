# Degree of turnover
# Data from Magurran 1988, p. 162
trees <- IncidenceMatrix(
  data = c(1, 1, 1, 0, 0, 0,
           1, 1, 1, 1, 1, 1,
           0, 0, 1, 0, 1, 0,
           0, 0, 0, 1, 1, 1,
           0, 0, 0, 0, 1, 1,
           0, 0, 0, 1, 0, 1),
  nrow = 6, byrow = FALSE,
  dimnames = list(c("1", "2", "3", "4", "5", "6"),
                  c("Birch", "Oak", "Rowan", "Beech", "Hazel", "Holly"))
)

## Whittaker's measure
turnover(trees, "whittaker") # 1

## Cody's measure
turnover(trees, "cody") # 3

## Routledge's measures
turnover(trees, "routledge1") # 0.29
turnover(trees, "routledge2") # 0.56
turnover(trees, "routledge3") # 1.75

## Wilson and Shmida's measure
turnover(trees, "wilson") # 1

# Similarity measures
# Data from Magurran 1988, p. 166
birds <- CountMatrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9) * 10,
  nrow = 2, byrow = TRUE, dimnames = list(c("unmanaged", "managed"), NULL)
)

## Jaccard measure
## (presence/absence data)
similarity(birds, "jaccard") # 0.46

## Sorenson measure
## (presence/absence data)
similarity(birds, "sorenson") # 0.63

## Jaccard measure (Bray's formula)
## (count data)
similarity(birds, "bray") # 0.44

## Morisita-Horn measure
## (count data)
similarity(birds, "morisita") # 0.81
