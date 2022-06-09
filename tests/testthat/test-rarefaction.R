# Rarefaction ==================================================================
test_that("Rarefaction", {
  # Data from Magurran 1988, p. 128-129
  trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                          1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                 nrow = 2, byrow = TRUE,
                 dimnames = list(c(1, 2), NULL))
  expect_snapshot(rarefaction(trap, sample = 13))
})

# Indices ======================================================================
test_that("Hurlbert rarefaction", {
  # Magurran 1988, p. 128
  n1 <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  n2 <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_snapshot(index_hurlbert(n1, 13)) # 6.58
})
