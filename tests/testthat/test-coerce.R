context("Coerce an object to a class")
library(tabula)
options("verbose" = TRUE)

# Coerce from matrix ===========================================================
test_count <- matrix(sample(0:10, 50, TRUE), ncol = 10)
test_freq <- test_count / rowSums(test_count)
test_incid <- matrix(sample(0:1, 50, TRUE), ncol = 10)

test_that("Coerce from matrix to CountMatrix", {
  expect_s4_class(as(test_count, "CountMatrix"), "CountMatrix")
  expect_s4_class(as(test_freq, "CountMatrix"), "CountMatrix")
  # expect_error(as(test_freq, "CountMatrix"))
  expect_error(as(test_incid, "CountMatrix"))
})
test_that("Coerce from matrix to FrequencyMatrix", {
  expect_s4_class(as(test_freq, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(test_count, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(test_incid, "FrequencyMatrix"), "FrequencyMatrix")
})
test_that("Coerce from matrix to IncidenceMatrix", {
  expect_s4_class(as(test_incid, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(test_count, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(test_freq, "IncidenceMatrix"), "IncidenceMatrix")
})
# Coerce from data.frame =======================================================
test_count <- as.data.frame(matrix(sample(0:10, 50, TRUE), ncol = 10))
test_freq <- as.data.frame(test_count / rowSums(test_count))
test_incid <- as.data.frame(matrix(sample(0:1, 50, TRUE), ncol = 10))

test_that("Coerce from data.frame to CountMatrix", {
  expect_s4_class(as(test_count, "CountMatrix"), "CountMatrix")
  expect_s4_class(as(test_freq, "CountMatrix"), "CountMatrix")
  # expect_error(as(test_freq, "CountMatrix"))
  expect_error(as(test_incid, "CountMatrix"))
})
test_that("Coerce from data.frame to FrequencyMatrix", {
  expect_s4_class(as(test_count, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(test_freq, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(test_incid, "FrequencyMatrix"), "FrequencyMatrix")
})
test_that("Coerce from data.frame to IncidenceMatrix", {
  expect_s4_class(as(test_count, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(test_freq, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(test_incid, "IncidenceMatrix"), "IncidenceMatrix")
})
# Coerce from *Matrix ==========================================================
test_count <- as(matrix(sample(0:10, 50, TRUE), ncol = 10), "CountMatrix")
test_freq <- as(test_count / rowSums(test_count), "FrequencyMatrix")
test_incid <- as(matrix(sample(0:1, 50, TRUE), ncol = 10), "IncidenceMatrix")

test_that("Coerce from *Matrix to data.frame", {
  expect_is(as(test_count, "data.frame"), "data.frame")
  expect_is(as(test_freq, "data.frame"), "data.frame")
  expect_is(as(test_incid, "data.frame"), "data.frame")
})
test_that("Coerce from *Matrix to CountMatrix", {
  expect_s4_class(as(test_count, "CountMatrix"), "CountMatrix")
  expect_s4_class(as(test_freq, "CountMatrix"), "CountMatrix")
  expect_error(as(test_incid, "CountMatrix"))
})
test_that("Coerce from *Matrix to CountMatrix", {
  count1 <- as(test_count, "CountMatrix")
  freq <- as(count1, "FrequencyMatrix")
  count2 <- as(freq, "CountMatrix")
  expect_equal(count1, count2)
})
test_that("Coerce from *Matrix to FrequencyMatrix", {
  expect_s4_class(as(test_count, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(test_freq, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(test_incid, "FrequencyMatrix"), "FrequencyMatrix")
})
test_that("Coerce from *Matrix to IncidenceMatrix", {
  expect_s4_class(as(test_count, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(test_freq, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(test_incid, "IncidenceMatrix"), "IncidenceMatrix")
})
