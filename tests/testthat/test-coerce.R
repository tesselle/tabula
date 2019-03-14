context("Coerce")

# matrix =======================================================================
mtx_count <- matrix(sample(0:10, 50, TRUE), ncol = 10)
mtx_freq <- mtx_count / rowSums(mtx_count)
mtx_incid <- matrix(as.logical(sample(0:1, 50, TRUE)), ncol = 10)
mtx_sim <- matrix(1, nrow = 5, ncol = 5)

test_that("matrix <> CountMatrix", {
  A <- as(mtx_count, "CountMatrix")
  expect_s4_class(A, "CountMatrix")
  expect_equal(as(A, "matrix"), mtx_count)

  expect_error(as(mtx_freq, "CountMatrix"))
  expect_error(as(mtx_incid, "CountMatrix"))
})
test_that("matrix <> FrequencyMatrix", {
  B <- as(mtx_freq, "FrequencyMatrix")
  expect_s4_class(B, "FrequencyMatrix")
  expect_equal(as(B, "matrix"), mtx_freq)

  expect_s4_class(as(mtx_count, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(mtx_incid, "FrequencyMatrix"), "FrequencyMatrix")
})
test_that("matrix <> IncidenceMatrix", {
  C <- as(mtx_incid, "IncidenceMatrix")
  expect_s4_class(C, "IncidenceMatrix")
  expect_equal(as(C, "matrix"), mtx_incid)

  expect_s4_class(as(mtx_count, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(mtx_freq, "IncidenceMatrix"), "IncidenceMatrix")
})
test_that("matrix <> OccurrenceMatrix", {
  D <- as(mtx_incid, "OccurrenceMatrix")
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as(D, "matrix"), "matrix")

  expect_s4_class(as(mtx_count, "OccurrenceMatrix"), "OccurrenceMatrix")
  expect_s4_class(as(mtx_freq, "OccurrenceMatrix"), "OccurrenceMatrix")
})
test_that("matrix <> SimilarityMatrix", {
  E <- as(mtx_sim, "SimilarityMatrix")
  expect_s4_class(E, "SimilarityMatrix")
  expect_is(as(E, "matrix"), "matrix")
})

# data.frame ===================================================================
df_count <- as.data.frame(mtx_count)
df_freq <- as.data.frame(mtx_freq)
df_incid <- as.data.frame(mtx_incid)
df_sim <- as.data.frame(mtx_sim)

test_that("data.frame <> CountMatrix", {
  A <- as(df_count, "CountMatrix")
  expect_s4_class(A, "CountMatrix")
  expect_identical(as(A, "data.frame"), df_count)

  expect_error(as(df_freq, "CountMatrix"))
  expect_error(as(df_incid, "CountMatrix"))
})
test_that("data.frame <> FrequencyMatrix", {
  B <- as(df_freq, "FrequencyMatrix")
  expect_s4_class(B, "FrequencyMatrix")
  # expect_identical(as(B, "data.frame"), df_freq)

  expect_s4_class(as(df_count, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(df_incid, "FrequencyMatrix"), "FrequencyMatrix")
})
test_that("data.frame <> IncidenceMatrix", {
  C <- as(df_incid, "IncidenceMatrix")
  expect_s4_class(C, "IncidenceMatrix")
  expect_identical(as(C, "data.frame"), df_incid)

  expect_s4_class(as(df_count, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(df_freq, "IncidenceMatrix"), "IncidenceMatrix")
})
test_that("data.frame <> OccurrenceMatrix", {
  D <- as(df_incid, "OccurrenceMatrix")
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as(D, "data.frame"), "data.frame")

  expect_s4_class(as(df_count, "OccurrenceMatrix"), "OccurrenceMatrix")
  expect_s4_class(as(df_freq, "OccurrenceMatrix"), "OccurrenceMatrix")
})
test_that("data.frame <> SimilarityMatrix", {
  E <- as(df_sim, "SimilarityMatrix")
  expect_s4_class(E, "SimilarityMatrix")
  expect_is(as(E, "data.frame"), "data.frame")
})

# *Matrix ======================================================================
count <- as(mtx_count, "CountMatrix")
freq <- as(mtx_freq, "FrequencyMatrix")
incid <- as(mtx_incid, "IncidenceMatrix")
occ <- as(mtx_incid, "OccurrenceMatrix")

test_that("CountMatrix <> FrequencyMatrix", {
  count1 <- as(mtx_count, "CountMatrix")
  freq1 <- as(count1, "FrequencyMatrix")
  count2 <- as(freq1, "CountMatrix")
  expect_identical(count1, count2)
})
test_that("*Matrix > CountMatrix", {
  expect_s4_class(as(count, "CountMatrix"), "CountMatrix")
  expect_error(as(freq, "CountMatrix"))
  expect_error(as(incid, "CountMatrix"))
  # expect_error(as(occ, "CountMatrix"))
})
test_that("*Matrix > FrequencyMatrix", {
  expect_s4_class(as(count, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(freq, "FrequencyMatrix"), "FrequencyMatrix")
  expect_s4_class(as(incid, "FrequencyMatrix"), "FrequencyMatrix")
  # expect_error(as(occ, "FrequencyMatrix"))
})
test_that("*Matrix > IncidenceMatrix", {
  expect_s4_class(as(count, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(freq, "IncidenceMatrix"), "IncidenceMatrix")
  expect_s4_class(as(incid, "IncidenceMatrix"), "IncidenceMatrix")
  # expect_error(as(occ, "IncidenceMatrix"))
})
test_that("*Matrix > OccurrenceMatrix", {
  expect_s4_class(as(count, "OccurrenceMatrix"), "OccurrenceMatrix")
  expect_s4_class(as(freq, "OccurrenceMatrix"), "OccurrenceMatrix")
  expect_s4_class(as(incid, "OccurrenceMatrix"), "OccurrenceMatrix")
})
