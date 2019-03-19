context("Richness")

# Richness =====================================================================
test_that("Richness", {
  # Data from Magurran 1988, p. 128-129
  trap <- CountMatrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                               1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                      nrow = 2, byrow = TRUE)
  method <- c("margalef", "menhinick", "none")
  expected <- matrix(data = c(2.55, 1.88, 9,
                              1.95, 1.66, 6),
                     nrow = 2, byrow = TRUE,
                     dimnames = list(c(1,2), method))

  index <- richness(trap, method, simplify = TRUE)
  expect_equal(round(index, digits = 2), expected)

  # Frequency data
  freq <- as(trap, "FrequencyMatrix")
  expect_error(richness(freq))

  # Incidence data
  incid <- as(trap, "IncidenceMatrix")
  method <- c("chao2", "ice")
  expect_is(richness(incid, method, simplify = TRUE), "numeric")
})

# Indices ======================================================================
test_that("Margalef richness", {
  expect_error(margalefRichness(LETTERS))

  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(round(margalefRichness(n), 2), 5.47)
  # Magurran 1988, p. 128
  n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  expect_equal(round(margalefRichness(n), 2), 2.55)
  n <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(margalefRichness(n), 2), 1.95)
})
test_that("Menhinick richness", {
  expect_error(menhinickRichness(LETTERS))

  # Magurran 1988, p. 128
  n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  expect_equal(round(menhinickRichness(n), 2), 1.88)
  n <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(menhinickRichness(n), 2), 1.66)
})

# Data from Chao & Chiu (2016)
edge <- rep(x = c(1:21, 23, 25, 27, 28, 30, 32, 34:37, 41,
                  45, 46, 49, 52, 89, 110, 123, 140),
            times = c(113, 50, 39, 29, 15, 11, 13, 5, 6, 6, 3, 4, 3, 5,
                      2, 5, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1,
                      0, 0, 2, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0))
interior <- rep(x = c(1:21, 23, 25, 27, 28, 30, 32, 34:37, 41,
                      45, 46, 49, 52, 89, 110, 123, 140),
                times = c(129, 49, 42, 32, 19, 17, 7, 9, 7, 7, 6, 3, 3, 3,
                          4, 4, 2, 2, 3, 4, 6, 2, 1, 2, 1, 1, 1,
                          1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1))

test_that("Chao1-type estimators", {
  expect_error(chao1Richness(LETTERS))
  expect_error(chao1iRichness(LETTERS))

  expect_equal(round(chao1Richness(edge, unbiased = FALSE), 3), 461.625)
  expect_equal(round(chao1Richness(edge, unbiased = TRUE), 3), 458.016)
  expect_equal(round(chao1iRichness(edge, unbiased = FALSE), 3), 488.284) # 488.313

  expect_equal(round(chao1Richness(interior, unbiased = FALSE), 3), 540.728)
  expect_equal(round(chao1Richness(interior, unbiased = TRUE), 3), 536.044)
  expect_equal(round(chao1iRichness(interior, unbiased = FALSE), 3), 572.471) # 572.505
})
test_that("ACE-type estimators", {
  expect_error(aceRichness(LETTERS))

  expect_equal(round(aceRichness(edge, k = 10), 3), 445.822) # 443.684
  expect_equal(round(aceRichness(interior, k = 10), 3), 501.045) # 498.834
})
test_that("Chao2-type estimators", {
  expect_error(chao2Richness(LETTERS))
  expect_error(chao2iRichness(LETTERS))

  a <- matrix(as.logical(
    c(0, 1, 1, 1,
      1, 0, 0, 1,
      0, 0, 1, 1)),
    nrow = 3, ncol = 4, byrow = TRUE
  )
  b <- matrix(as.logical(
    c(0, 1, 1, 1,
      1, 0, 0, 1,
      0, 0, 0, 1)),
    nrow = 3, ncol = 4, byrow = TRUE
  )
  c <- matrix(
    as.logical(c(0, 1, 1, 1,
                 1, 0, 0, 1,
                 0, 0, 1, 1,
                 1, 0, 1, 1)),
    nrow = 4, ncol = 4, byrow = TRUE
  )
  expect_equal(round(chao2Richness(a, unbiased = TRUE), 2), 4.33)
  expect_equal(round(chao2Richness(a, unbiased = FALSE), 2), 5.33)
  expect_equal(round(chao2Richness(b, unbiased = FALSE), 2), 6.00)
  expect_equal(round(chao2iRichness(c, unbiased = TRUE), 2), 4.05)
  expect_equal(round(chao2iRichness(c, unbiased = FALSE), 2), 4.43)
  expect_error(chao2iRichness(a, unbiased = FALSE))
})
test_that("ICE-type estimators", {
  expect_error(iceRichness(LETTERS))

  a <- t(matrix(as.logical(c(0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1)), 4, 3))
  expect_equal(round(iceRichness(a, k = 10), 1), 5.6)
})
