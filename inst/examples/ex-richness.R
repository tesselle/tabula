## Richness
## Margalef and Menhinick index
## Data from Magurran 1988, p. 128-129
trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                        1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
               nrow = 2, byrow = TRUE, dimnames = list(c("A", "B"), NULL))
richness(trap, method = "margalef") # 2.55 1.88
richness(trap, method = "menhinick") # 1.95 1.66

## Asymptotic species richness
## Chao1-type estimators
## Data from Chao & Chiu 2016
brazil <- matrix(
  data = rep(x = c(1:21, 23, 25, 27, 28, 30, 32, 34:37, 41,
                   45, 46, 49, 52, 89, 110, 123, 140),
             times = c(113, 50, 39, 29, 15, 11, 13, 5, 6, 6, 3, 4,
                       3, 5, 2, 5, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1,
                       0, 0, 2, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0)),
  nrow = 1, byrow = TRUE
)

composition(brazil, method = c("chao1"), unbiased = FALSE) # 461.625
composition(brazil, method = c("ace"), k = 10) # 445.822

## Rarefaction
rarefaction(trap, sample = 13) # 6.56 6.00
