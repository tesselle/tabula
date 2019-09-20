context("Coerce")

# matrix =======================================================================
mtx_count <- matrix(sample(0:10, 50, TRUE), ncol = 10)
mtx_freq <- mtx_count / rowSums(mtx_count)
mtx_incid <- matrix(as.logical(sample(0:1, 50, TRUE)), ncol = 10)
mtx_sim <- matrix(1, nrow = 5, ncol = 5)

test_that("matrix <> CountMatrix", {
  A <- as_count(mtx_count)
  expect_s4_class(A, "CountMatrix")
  expect_equal(as(A, "matrix"), mtx_count)

  expect_message(as_count(mtx_freq))
  expect_message(as_count(mtx_incid))
})
test_that("matrix <> FrequencyMatrix", {
  B <- as_frequency(mtx_freq)
  expect_s4_class(B, "FrequencyMatrix")
  expect_equal(as(B, "matrix"), mtx_freq)

  expect_s4_class(as_frequency(mtx_count), "FrequencyMatrix")
  expect_s4_class(as_frequency(mtx_incid), "FrequencyMatrix")
})
test_that("matrix <> IncidenceMatrix", {
  C <- as_incidence(mtx_incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equal(as(C, "matrix"), mtx_incid)

  expect_s4_class(as_incidence(mtx_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(mtx_freq), "IncidenceMatrix")
})
test_that("matrix <> OccurrenceMatrix", {
  D <- as_occurrence(mtx_incid)
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as(D, "matrix"), "matrix")

  expect_s4_class(as_occurrence(mtx_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(mtx_freq), "OccurrenceMatrix")
})
test_that("matrix <> SimilarityMatrix", {
  E <- as_similarity(mtx_sim)
  expect_s4_class(E, "SimilarityMatrix")
  expect_is(as(E, "matrix"), "matrix")
})

# data.frame ===================================================================
df_count <- as.data.frame(mtx_count)
df_freq <- as.data.frame(mtx_freq)
df_incid <- as.data.frame(mtx_incid)
df_sim <- as.data.frame(mtx_sim)

test_that("data.frame <> CountMatrix", {
  A <- as_count(df_count)
  expect_s4_class(A, "CountMatrix")
  expect_equal(as(A, "data.frame"), df_count)

  expect_message(as_count(df_freq))
  expect_message(as_count(df_incid))
})
test_that("data.frame <> FrequencyMatrix", {
  B <- as_frequency(df_freq)
  expect_s4_class(B, "FrequencyMatrix")
  expect_equal(as(B, "data.frame"), df_freq)

  expect_s4_class(as_frequency(df_count), "FrequencyMatrix")
  expect_s4_class(as_frequency(df_incid), "FrequencyMatrix")
})
test_that("data.frame <> IncidenceMatrix", {
  C <- as_incidence(df_incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_identical(as(C, "data.frame"), df_incid)

  expect_s4_class(as_incidence(df_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(df_freq), "IncidenceMatrix")
})
test_that("data.frame <> OccurrenceMatrix", {
  D <- as_occurrence(df_incid)
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as(D, "data.frame"), "data.frame")

  expect_s4_class(as_occurrence(df_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(df_freq), "OccurrenceMatrix")
})
test_that("data.frame <> SimilarityMatrix", {
  E <- as_similarity(df_sim)
  expect_s4_class(E, "SimilarityMatrix")
  expect_is(as(E, "data.frame"), "data.frame")
})

# *Matrix ======================================================================
count <- as(mtx_count, "CountMatrix")
freq <- as(mtx_freq, "FrequencyMatrix")
incid <- as(mtx_incid, "IncidenceMatrix")
occ <- as(mtx_incid, "OccurrenceMatrix")

test_that("CountMatrix <> FrequencyMatrix", {
  count1 <- as_count(mtx_count)
  freq1 <- as_frequency(count1)
  count2 <- as_count(freq1)
  expect_identical(count1, count2)

  freq1@totals <- numeric(0)
  expect_error(as_count(freq1), "Cannot calculate absolute frequencies")
})
test_that("*Matrix > CountMatrix", {
  expect_s4_class(as_count(count), "CountMatrix")
  expect_message(as_count(freq))
  expect_message(as_count(incid))
  # expect_error(as_count(occ))
})
test_that("*Matrix > FrequencyMatrix", {
  expect_s4_class(as_frequency(count), "FrequencyMatrix")
  expect_s4_class(as_frequency(freq), "FrequencyMatrix")
  expect_s4_class(as_frequency(incid), "FrequencyMatrix")
  # expect_error(as_frequency(occ))
})
test_that("*Matrix > IncidenceMatrix", {
  expect_s4_class(as_incidence(count), "IncidenceMatrix")
  expect_s4_class(as_incidence(freq), "IncidenceMatrix")
  expect_s4_class(as_incidence(incid), "IncidenceMatrix")
  # expect_error(as_incidence(occ))
})
test_that("*Matrix > OccurrenceMatrix", {
  expect_s4_class(as_occurrence(count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(freq), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(incid), "OccurrenceMatrix")
})
