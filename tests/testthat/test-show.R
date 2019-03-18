context("Show")

test_that("BootCA", {
  A <- methods::new("BootCA")
  expect_output(show(A), "Partial bootstrap CA seriation refinement:")
})
test_that("DateModel", {
  A <- methods::new("DateModel")
  expect_output(show(A), "Modelled event date")
})
test_that("Logical matrix", {
  A <- methods::new("LogicalMatrix")
  expect_output(show(A), "An object of class \"LogicalMatrix\"")

  B <- methods::new("IncidenceMatrix")
  expect_output(show(B), "presence/absence data matrix")

  C <- methods::new("OccurrenceMatrix")
  expect_output(show(C), "co-occurrence matrix")
})
test_that("Numeric matrix", {
  A <- methods::new("NumericMatrix")
  expect_output(show(A), "An object of class \"NumericMatrix\"")

  B <- methods::new("CountMatrix")
  expect_output(show(B), "count data matrix")

  C <- methods::new("FrequencyMatrix")
  expect_output(show(C), "frequency data matrix")

  D <- methods::new("SimilarityMatrix")
  expect_output(show(D), "similarity matrix")
})
test_that("PermutationOrder", {
  A <- methods::new("PermutationOrder")
  expect_output(show(A), "Permutation order for matrix seriation:")

  B <- methods::new("PermutationOrder",
                    rows = 1:21, columns = 1:21, method = "XXX")
  expect_output(show(B), "Permutation order for matrix seriation:")
})
