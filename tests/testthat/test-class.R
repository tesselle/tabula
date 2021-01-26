context("Extract")

test_that("BootCA", {
  cls_bootCA <- new("BootCA")

  expect_type(cls_bootCA[["row_chull"]], "list")
  expect_type(cls_bootCA[["column_chull"]], "list")
  expect_type(cls_bootCA[["lengths"]], "list")
  expect_type(cls_bootCA[["cutoff"]], "double")
  expect_type(cls_bootCA[["keep"]], "list")
  expect_error(cls_bootCA[["X"]])

  expect_s3_class(cls_bootCA["row", , drop = FALSE], "data.frame")
  expect_s3_class(cls_bootCA["row_chull", NULL , drop = FALSE], "data.frame")
  expect_s3_class(cls_bootCA["row_chull", 1 , drop = FALSE], "data.frame")
  expect_s3_class(cls_bootCA["row_chull", "a" , drop = FALSE], "data.frame")
  expect_s3_class(cls_bootCA["column_chull",  , drop = FALSE], "data.frame")
  expect_error(cls_bootCA["X", ])
})
test_that("DateModel", {
  date_model <- new("DateModel")

  expect_type(date_model[["data"]], "double")
  expect_type(date_model[["dates"]], "double")
  expect_type(date_model[["model"]], "S4")
  expect_type(date_model[["dimension"]], "integer")
  expect_error(date_model[["X"]])

  expect_type(date_model["data", , drop = FALSE], "double")
  expect_type(date_model["data", NULL, drop = FALSE], "double")
  expect_type(date_model["data", 1, drop = TRUE], "double")
  expect_type(date_model["data", 1:2], "double")
  expect_type(date_model["data", "A"], "double")
  expect_error(date_model["X", ])
})
test_that("DateEvent", {
  date_model <- new("DateEvent")

  expect_type(date_model[["data"]], "double")
  expect_type(date_model[["level"]], "double")
  expect_type(date_model[["row_events"]], "double")
  expect_type(date_model[["column_events"]], "double")
  expect_type(date_model[["accumulation"]], "double")
  expect_error(date_model[["X"]])

  expect_type(date_model["data", , drop = FALSE], "double")
  expect_type(date_model["data", NULL, drop = FALSE], "double")
  expect_type(date_model["data", 1, drop = TRUE], "double")
  expect_type(date_model["data", 1:2], "double")
  expect_type(date_model["data", "A"], "double")
  expect_type(date_model["row", ], "double")
  expect_type(date_model["row", NULL], "double")
  expect_type(date_model["row", 1], "double")
  expect_type(date_model["column", ], "double")
  expect_type(date_model["accumulation", ], "double")
  expect_error(date_model["X", ])
})
test_that("PermutationOrder", {
  perm_order <- new("PermutationOrder")

  expect_type(perm_order[["rows"]], "integer")
  expect_type(perm_order[["columns"]], "integer")
  expect_type(perm_order[["method"]], "character")
})
