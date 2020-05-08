context("Extract")

test_that("BootCA", {
  boot_ca <- new("BootCA")

  expect_type(boot_ca[["row_chull"]], "list")
  expect_type(boot_ca[["column_chull"]], "list")
  expect_type(boot_ca[["lengths"]], "list")
  expect_type(boot_ca[["cutoff"]], "double")
  expect_type(boot_ca[["keep"]], "list")
  expect_error(boot_ca[["X"]])

  expect_s3_class(boot_ca["row", , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["row_chull", NULL , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["row_chull", 1 , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["row_chull", "a" , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["column_chull",  , drop = FALSE], "data.frame")
  expect_error(boot_ca["X", ])
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
