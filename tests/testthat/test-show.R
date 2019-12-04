context("Show")

test_that("BootCA", {
  A <- methods::new("BootCA")
  expect_output(show(A), "Partial bootstrap CA seriation refinement:")
})
test_that("DateModel", {
  A <- methods::new("DateModel")
  expect_output(show(A), "Modelled event date")
})
test_that("PermutationOrder", {
  A <- methods::new("PermutationOrder")
  expect_output(show(A), "Permutation order for matrix seriation:")

  B <- methods::new("PermutationOrder",
                    rows = 1:21, columns = 1:21, method = "XXX")
  expect_output(show(B), "Permutation order for matrix seriation:")
})
