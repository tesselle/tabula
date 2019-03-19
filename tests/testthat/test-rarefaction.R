context("Rarefaction")

# Rarefaction ==================================================================
test_that("Rarefaction", {
  # Data from Magurran 1988, p. 128-129
  trap <- CountMatrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                               1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                      nrow = 2, byrow = TRUE)
  expected <- c(`1` = 6.56, `2` = NA)
  index <- rarefaction(trap, 13)

  expect_equal(round(index, digits = 2), expected)

  freq <- as(trap, "FrequencyMatrix")
  expect_error(rarefaction(freq, 13))
  incid <- as(trap, "IncidenceMatrix")
  expect_error(rarefaction(incid, 13))
})

# Indices ======================================================================
test_that("Hurlbert rarefaction", {
  expect_error(hurlbertRarefaction(LETTERS, 13))

  # Magurran 1988, p. 128
  n1 <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  n2 <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(hurlbertRarefaction(n1, 13), 2), 6.56) # 6.58
})
