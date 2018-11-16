context("alpha-diversity")
library(tabula)
options("verbose" = TRUE)

# Richness =====================================================================
test_that("Richness", {
  # Data from Magurran 1988, p. 128-129
  trap <- CountMatrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                               1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                      nrow = 2, byrow = TRUE)
  method <- c("margalef", "menhinick")
  expected <- matrix(data = c(2.55, 1.88, 1.95, 1.66), nrow = 2, byrow = TRUE,
                     dimnames = list(c(1,2), method))

  index <- richness(trap, method, simplify = TRUE)
  expect_identical(round(index, digits = 2), expected)

  freq <- as(trap, "FrequencyMatrix")
  expect_error(richness(freq))
  incid <- as(trap, "IncidenceMatrix")
  expect_error(richness(bin))
})

# Rarefaction ==================================================================
test_that("Rarefaction", {
  # Data from Magurran 1988, p. 128-129
  trap <- CountMatrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                               1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                      nrow = 2, byrow = TRUE)
  expected <- c(`1` = 6.56, `2` = NA)
  index <- rarefaction(trap, 13)

  expect_identical(round(index, digits = 2), expected)

  freq <- as(trap, "FrequencyMatrix")
  expect_error(rarefaction(freq, 13))
  incid <- as(trap, "IncidenceMatrix")
  expect_error(rarefaction(incid, 13))
})

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
