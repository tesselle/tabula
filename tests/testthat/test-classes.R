test_that("DateEvent", {
  expect_output(show(.DateEvent()), "<DateEvent>")
  cls_event <- new("DateEvent")

  expect_error(cls_event[["X"]])
})
test_that("DateMCD", {
  expect_output(show(.DateMCD()), "<DateMCD>")
  cls_mcd <- new("DateMCD")

  expect_error(cls_mcd[["X"]])
})
test_that("DiversityIndex", {
  expect_output(show(.DiversityIndex()), "<DiversityIndex: none>")
  cls_diversity <- new("DateEvent")

  expect_error(cls_diversity[["X"]])
})
test_that("IncrementTest", {
  expect_output(show(.IncrementTest()), "<IncrementTest>")
  cls_increment <- new("IncrementTest")

  expect_error(cls_increment[["X"]])
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
