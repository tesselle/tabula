# Create a count data matrix
A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                  nrow = 10, ncol = 10, byrow = TRUE)

# Access
dim(A1) # Get the matrix dimensions
colnames(A1) # Get the column names
colnames(A1) <- letters[11:20] # Set the column names
rownames(A1) # Get the rownames
rownames(A1) <- LETTERS[1:10] # Set the row names

# Subset
A1[["id"]] # Get the matrix ID
A1[, ] # Get all values
A1[1, ] # Get the first row
A1[c("A", "B", "C"), ] # Get the first three rows
A1[c("A", "B", "C"), 1] # Get the first three rows of the first column
A1[, 1, drop = FALSE] # Get the first column

# Coerce counts to frequencies
B <- as(A1, "FrequencyMatrix")
# Row sums are internally stored before coercing to a frequency matrix
get_totals(B) # Get row sums
# This allows to restore the source data
A2 <- as(B, "CountMatrix")
all(A1 == A2)
# Coerce to a co-occurrence matrix
C <- as(B, "OccurrenceMatrix")

# Set space-time information
## Geographic coordinates
set_epsg(A1) <- 4326
set_coordinates(A1) <- list(X = sample(0:10, 10, TRUE),
                            Y = sample(0:10, 10, TRUE))
get_coordinates(A1)
\donttest{
## Convert to a sf object (the sf package must be installed on your machine)
feat <- get_features(A1)
sf::st_as_sf(feat, crs = 4326, coords = c("X", "Y"), dim = "XY",
             remove = FALSE, na.fail = TRUE)
}
