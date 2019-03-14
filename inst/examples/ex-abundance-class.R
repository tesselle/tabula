# Create a count data matrix
A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                  nrow = 10, ncol = 10, byrow = TRUE)

# Coerce counts to frequencies
B <- as(A1, "FrequencyMatrix")

# Row sums are internally stored before coercing to a frequency matrix
totals(B) # Get row sums

# This allows to restore the source data
A2 <- as(B, "CountMatrix")
all(A1 == A2)

# Coerce to a co-occurrence matrix
C <- as(B, "OccurrenceMatrix")
