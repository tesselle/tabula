context("Classes")

test_that("Initialize a BootCA object", {
  expect_s4_class(.BootCA(), "BootCA")
})
test_that("Initialize a DateModel object", {
  expect_s4_class(.DateModel(), "DateModel")
})
test_that("Initialize a PermutationOrder object", {
  expect_s4_class(.PermutationOrder(), "PermutationOrder")
})
