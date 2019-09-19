## Create a count data matrix
A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                  nrow = 10, ncol = 10, byrow = TRUE)

## Access
get_id(A1)
dim(A1) # Get the matrix dimensions
colnames(A1) # Get the column names
colnames(A1) <- letters[11:20] # Set the column names
rownames(A1) # Get the rownames
rownames(A1) <- LETTERS[1:10] # Set the row names

## Subset
A1[["id"]] # Get the matrix ID
A1[, ] # Get all values
A1[1, ] # Get the first row
A1[c("A", "B", "C"), ] # Get the first three rows
A1[c("A", "B", "C"), 1] # Get the first three rows of the first column
A1[, 1, drop = FALSE] # Get the first column

## Coerce counts to frequencies
B <- as_frequency(A1)
## Row sums are internally stored before coercing to a frequency matrix
get_totals(B) # Get row sums
## This allows to restore the source data
A2 <- as_count(B)
all(A1 == A2)
