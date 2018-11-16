context("Seriation")
library(tabula)
options("verbose" = TRUE)

test_that("Reciprocal averaging on EPPM", {
  count <- as(compiegne, "CountMatrix")
  indices <- seriate(count, method = "reciprocal", EPPM = TRUE, margin = 2)
  expected <- c("N", "A", "C", "K", "P", "L", "B", "E",
                "I", "M", "D", "G", "O", "J", "F", "H")
  expect_identical(LETTERS[indices@columns], expected)
})


