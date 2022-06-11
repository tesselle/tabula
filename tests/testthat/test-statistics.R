test_that("Compute thresholds", {
  skip_if_not_installed("folio")
  data("compiegne", package = "folio")

  expect_snapshot(eppm(compiegne))
  expect_snapshot(pvi(compiegne))
})
test_that("Binomial coefficient", {
  expect_equal(combination(4, 3), 4)
  # Ramanujan factorial approx.
  expect_equal(combination(171, 3), 818816.247275706)
  expect_error(combination(3, "a"))

  options("verbose" = TRUE)
  expect_message(combination(171, 3), "Ramanujan approximation of x!")
})
test_that("Confidence interval for a proportion", {
  expect_type(confidence_proportion(1:10, alpha = 0.05, type = "norm"), "double")
  expect_type(confidence_proportion(1:10, alpha = 0.05, type = "stud"), "double")
  expect_length(confidence_proportion(1:10), 10)
  expect_error(confidence_proportion(LETTERS))
})
