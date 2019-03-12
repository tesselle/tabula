context("Show")

test_that("BootCA", {
  A <- methods::new("BootCA")
  expect_output(show(A), "Partial bootstrap CA seriation refinement:")
})
test_that("BootDate", {
  A <- methods::new("BootDate")
  expect_output(show(A), "Partial bootstrap date model refinement:")
})
test_that("DateModel", {
  A <- methods::new("DateModel")
  expect_output(show(A), "Modelled event date")
})
test_that("Logical matrix", {
  A <- methods::new("LogicalMatrix")
  expect_output(show(A), "An object of class \"LogicalMatrix\"")

  A <- methods::new("OccurrenceMatrix")
  expect_output(show(A), "co-occurrence matrix")
})
test_that("Numeric matrix", {
  A <- methods::new("NumericMatrix")
  expect_output(show(A), "An object of class \"NumericMatrix\"")

  A <- methods::new("CountMatrix")
  expect_output(show(A), "count data matrix")

  A <- methods::new("FrequencyMatrix")
  expect_output(show(A), "frequency data matrix")

  A <- methods::new("SimilarityMatrix")
  expect_output(show(A), "similarity matrix")
})
test_that("PermutationOrder", {
  A <- methods::new("PermutationOrder")
  expect_output(show(A), "Permutation order for matrix seriation:")
})
