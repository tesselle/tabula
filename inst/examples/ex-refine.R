## Data from Magurran 1988, p. 145-149
birds <- CountMatrix(
  data = c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3,
           3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 0, 0,
           30, 30, 3, 65, 20, 11, 0, 4, 2, 14,
           0, 3, 9, 0, 0, 5, 0, 0, 0, 0, 1, 1),
  nrow = 2, byrow = TRUE, dimnames = list(c("oakwood", "spruce"), NULL))

## Shannon diversity
h <- index_heterogeneity(birds, "shannon")
bootstrap(h)
jackknife(h)

## Shannon evenness
e <- index_evenness(birds, "shannon")
bootstrap(e)
jackknife(e)
