context("Show")

test_that("BootCA", {
  A <- methods::new("BootCA")
  expect_output(show(A), "BootCA")
})
test_that("DateModel", {
  A <- methods::new("DateModel")
  expect_output(show(A), "DateModel")
})
test_that("PermutationOrder", {
  A <- methods::new("PermutationOrder")
  expect_output(show(A), "PermutationOrder")
})
