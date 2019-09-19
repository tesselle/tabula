## Attempt to auto-coerce a matrix in a suitable way
mtx <- matrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10)
## Coerce to a count matrix
E <- as_matrix(mtx)
