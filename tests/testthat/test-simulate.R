context("Simulate Diversity")

test_that("Simulate Richness", {
  # Data from Kintigh 1989, p. 28
  magda <- c(0.9, 12.4, 3.6, 1.2, 0.3, 0.9, 3.6, 11.5, 2.7, 6.3,
             0.6, 0.6, 7.5, 3.3, 2.1, 3.3, 0.9, 1.2, 3.6, 1.8,
             1.5, 1.2, 2.1, 0.3, 2.4, 0.3, 0.6, 1.2, 0.6, 0.6,
             0.3, 0.3, 2.1, 0.6, 0.3, 0.3, 0.9, 0.3, 1.8, 0.9,
             2.7, 5.1, 3.0, 2.1)

  # Richness
  fun <- function(x, ...) { sum(x > 0) }
  index <- simulate_diversity(x = 1:70, method = fun,
                              prob = magda, level = 0.80, n = 500)

  expect_type(index, "double")
  expect_equal(dim(index), c(70, 4))
  expect_equivalent(round(index[70, 2]), 26, tolerance = 0.1)
})
test_that("Simulate Evenness", {
  # Data from Kintigh 1989, p. 32
  chevelon <- c(0.068, 0.029, 0.039, 0.117, 0.097,
                0.155, 0.015, 0.053, 0.257, 0.170)

  # Evenness
  index <- simulate_diversity(x = 1:30, method = evennessShannon,
                              prob = chevelon, level = 0.80, n = 1000)

  expect_type(index, "double")
  expect_equal(dim(index), c(30, 4))
  expect_equivalent(round(index[30, 2], 1), 0.9, tolerance = 0.1)
})
