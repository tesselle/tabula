## Create an incidence (presence/absence) matrix
## Data will be coerced with as.logical()
A <- IncidenceMatrix(data = sample(0:1, 100, TRUE, c(1, 0.3)),
                      nrow = 10, ncol = 10)
## Coerce to a co-occurrence matrix
B <- as_occurrence(A)
## Create a count data matrix
C <- CountMatrix(data = sample(0:10, 100, TRUE),
                 nrow = 10, ncol = 10, byrow = TRUE)
## Coerce to presence/absence
D <- as_incidence(C)

