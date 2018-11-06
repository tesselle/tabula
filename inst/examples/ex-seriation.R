# Coerce dataset to abundance matrix
# Data from Desachy 2004
count <- as(compiegne, "CountMatrix")

# Get seriation order for columns on EPPM using the reciprocal ranking method
indices <- seriate(count, method = "ranking", EPPM = TRUE, margin = 2)
# Permute columns
new_order <- permute(count, indices)

# Plot new matrix
plotBar(new_order, EPPM = TRUE)
