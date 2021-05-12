test_that("DiversityIndex", {
  expect_output(show(.DiversityIndex()), "<DiversityIndex: none>")
  cls_diversity <- new("DiversityIndex")

  expect_error(cls_diversity[["X"]])
})
test_that("PermutationOrder", {
  expect_output(show(.PermutationOrder()), "<PermutationOrder: none>")
  perm_order <- new("PermutationOrder")

  expect_error(perm_order[["X"]])
})
test_that("RefineCA", {
  expect_output(show(.RefineCA()), "<RefineCA>")
  cls_bootCA <- new("RefineCA")

  expect_type(cls_bootCA[["rows"]], "list")
  expect_type(cls_bootCA[["columns"]], "list")
  expect_error(cls_bootCA[["X"]])
})
