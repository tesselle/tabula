test_that("RefineCA", {
  expect_output(show(.RefineCA()), "<RefineCA>")
  cls_bootCA <- new("RefineCA")

  expect_type(cls_bootCA[["rows"]], "list")
  expect_type(cls_bootCA[["columns"]], "list")
  expect_error(cls_bootCA[["X"]])
})
