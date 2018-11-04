context("alpha-diversity")
library(tabula)
options("verbose" = TRUE)

# Diversity index ==============================================================
test_that("Diversity index - simplify = FALSE", {
  count <- as(compiegne, "CountMatrix")
  method <- c("berger", "brillouin", "mcintosh", "shannon", "simpson")
  index <- diversity(count, method = method, simplify = FALSE)
  expect_is(index, "list")
  expect_identical(length(index), length(method))
  expect_identical(unique(lengths(index)), nrow(count))
})
test_that("Diversity index - simplify = TRUE", {
  count <- as(compiegne, "CountMatrix")
  method <- c("berger", "brillouin", "mcintosh", "shannon", "simpson")
  index <- diversity(count, method = method, simplify = TRUE)
  expect_is(index, "matrix")
  expect_identical(dim(index), c(nrow(count), length(method)))
})

# Evenness =====================================================================
test_that("Evenness - simplify = FALSE", {
  count <- as(compiegne, "CountMatrix")
  method <- c("brillouin", "mcintosh", "shannon", "simpson")
  index <- evenness(count, method = method, simplify = FALSE)
  expect_is(index, "list")
  expect_identical(length(index), length(method))
  expect_identical(unique(lengths(index)), nrow(count))
})
test_that("Evenness - simplify = TRUE", {
  count <- as(compiegne, "CountMatrix")
  method <- c("brillouin", "mcintosh", "shannon", "simpson")
  index <- evenness(count, method = method, simplify = TRUE)
  expect_is(index, "matrix")
  expect_identical(dim(index), c(nrow(count), length(method)))
})

# Richness =====================================================================
test_that("Richness - Count data", {
  count <- as(compiegne, "CountMatrix")
  method <- c("margalef", "menhinick")
  index <- richness(count, method, simplify = TRUE)
  expect_is(index, "matrix")
  expect_identical(dim(index), c(nrow(count), length(method)))
})
test_that("Richness - Incidence data", {
  bin <- as(compiegne, "IncidenceMatrix")
  index <- richness(bin)
  expect_is(index, "integer")
  expect_identical(length(index), nrow(bin))
})

# Rarefaction ==================================================================
test_that("Rarefaction", {
  count <- as(compiegne, "CountMatrix")
  index <- rarefaction(count, 10)
  expect_is(index, "numeric")
  expect_identical(length(index), nrow(count))
})
