context("Extract")

test_that("RefineCA", {
  expect_s4_class(.RefineCA(), "RefineCA")
  cls_bootCA <- new("RefineCA")

  expect_type(cls_bootCA[["rows"]], "list")
  expect_type(cls_bootCA[["columns"]], "list")
  expect_type(cls_bootCA[["rows"]][["chull"]], "double")
  expect_type(cls_bootCA[["rows"]][["length"]], "double")
  expect_type(cls_bootCA[["rows"]][["keep"]], "double")
  expect_type(cls_bootCA[["cutoff"]], "double")
  expect_error(cls_bootCA[["X"]])
})
test_that("DateEvent", {
  date_model <- new("DateEvent")

  expect_type(date_model[["dates"]], "double")
  expect_type(date_model[["model"]], "S4")
  expect_type(date_model[["cutoff"]], "integer")
  expect_type(date_model[["keep"]], "integer")
  expect_error(date_model[["X"]])
})
test_that("PermutationOrder", {
  expect_s4_class(.PermutationOrder(), "PermutationOrder")
  perm_order <- new("PermutationOrder")

  expect_type(perm_order[["rows"]], "integer")
  expect_type(perm_order[["columns"]], "integer")
  expect_type(perm_order[["method"]], "character")
})
