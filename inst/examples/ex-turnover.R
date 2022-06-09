## Data from Magurran 1988, p. 162
trees <- matrix(
  data = c(1, 1, 1, 0, 0, 0,
           1, 1, 1, 1, 1, 1,
           0, 0, 1, 0, 1, 0,
           0, 0, 0, 1, 1, 1,
           0, 0, 0, 0, 1, 1,
           0, 0, 0, 1, 0, 1),
  nrow = 6, byrow = FALSE
)
colnames(trees) <- c("Birch", "Oak", "Rowan", "Beech", "Hazel", "Holly")

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
