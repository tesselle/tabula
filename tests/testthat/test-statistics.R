test_that("Compute thresholds", {
  data("cantabria")

  expect_snapshot(eppm(cantabria))
  expect_snapshot(pvi(cantabria))
})
test_that("Binomial coefficient", {
  expect_equal(combination(4, 3), 4)
  # Ramanujan factorial approx.
  expect_equal(combination(171, 3), 818816.247275706)
  expect_error(combination(3, "a"))

  options("tabula.verbose" = TRUE)
  expect_message(combination(171, 3), "Ramanujan approximation of x!")
})
