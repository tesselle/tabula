test_that("RefineCA", {
  A <- methods::new("RefineCA")
  expect_output(show(A), "RefineCA")
})
test_that("DateEvent", {
  A <- methods::new("DateEvent")
  expect_output(show(A), "DateEvent")
})
test_that("PermutationOrder", {
  A <- methods::new("PermutationOrder")
  expect_output(show(A), "PermutationOrder")
})
