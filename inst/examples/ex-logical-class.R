# Create an incidence (presence/absence) matrix
# Data will be coerced with as.logical()
C1 <- IncidenceMatrix(data = sample(0:1, 100, TRUE),
                      nrow = 10, ncol = 10)

# Coerce to a co-occurrence matrix
D <- as(C1, "OccurrenceMatrix")

# Create a count data matrix
A <- CountMatrix(data = sample(0:10, 100, TRUE),
                 nrow = 10, ncol = 10, byrow = TRUE)

# Coerce to presence/absence
C2 <- as(A, "IncidenceMatrix")
