context("Richness")

# Richness =====================================================================
test_that("Richness", {
  # Data from Magurran 1988, p. 128-129
  trap <- CountMatrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                               1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                      nrow = 2, byrow = TRUE)
  method <- c("margalef", "menhinick", "none")
  for (i in method) {
    index <- index_richness(trap, method = i)
    expect_s4_class(index, "RichnessIndex")
    expect_length(index@index, 2)
  }

  # Frequency data
  freq <- as(trap, "AbundanceMatrix")
  expect_error(index_richness(freq))
})
test_that("Chao richness", {
  # Data from Magurran 1988, p. 128-129
  trap <- CountMatrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                               1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                      nrow = 2, byrow = TRUE)
  # Incidence data
  incid <- as(trap, "IncidenceMatrix")
  method <- c("chao2", "ice")
  for (i in method) {
    index <- index_composition(incid, method = i)
    expect_s4_class(index, "RichnessIndex")
    expect_length(index@index, 1)
  }
})
# Indices ======================================================================
test_that("Margalef richness", {
  expect_error(richnessMargalef(LETTERS))

  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(round(richnessMargalef(n), 2), 5.47)
  # Magurran 1988, p. 128
  n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  expect_equal(round(richnessMargalef(n), 2), 2.55)
  n <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(richnessMargalef(n), 2), 1.95)
})
test_that("Menhinick richness", {
  expect_error(richnessMenhinick(LETTERS))

  # Magurran 1988, p. 128
  n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  expect_equal(round(richnessMenhinick(n), 2), 1.88)
  n <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_equal(round(richnessMenhinick(n), 2), 1.66)
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
  expect_error(richnessChao1(LETTERS))
  expect_error(chao1iRichness(LETTERS))

  expect_equal(round(richnessChao1(edge, unbiased = FALSE), 3), 461.625)
  expect_equal(round(richnessChao1(edge, unbiased = TRUE), 3), 458.016)
  expect_equal(round(richnessChao1(edge, unbiased = FALSE, improved = TRUE), 3), 488.284) # 488.313

  expect_equal(round(richnessChao1(interior, unbiased = FALSE), 3), 540.728)
  expect_equal(round(richnessChao1(interior, unbiased = TRUE), 3), 536.044)
  expect_equal(round(richnessChao1(interior, unbiased = FALSE, improved = TRUE), 3), 572.471) # 572.505
})
test_that("ACE-type estimators", {
  expect_error(richnessACE(LETTERS))

  expect_equal(round(richnessACE(edge, k = 10), 3), 445.822) # 443.684
  expect_equal(round(richnessACE(interior, k = 10), 3), 501.045) # 498.834
})
test_that("Chao2-type estimators", {
  expect_error(richnessChao2(LETTERS))
  expect_error(richnessChao2(LETTERS, improved = TRUE))

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
  expect_equal(round(richnessChao2(a, unbiased = TRUE), 2), 4.33)
  expect_equal(round(richnessChao2(a, unbiased = FALSE), 2), 5.33)
  expect_equal(round(richnessChao2(b, unbiased = FALSE), 2), 6.00)
  expect_equal(round(richnessChao2(c, unbiased = TRUE, improved = TRUE), 2), 4.05)
  expect_equal(round(richnessChao2(c, unbiased = FALSE, improved = TRUE), 2), 4.43)
  expect_error(richnessChao2(a, unbiased = FALSE, improved = TRUE))
})
test_that("ICE-type estimators", {
  expect_error(iceRichness(LETTERS))

  a <- t(matrix(as.logical(c(0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1)), 4, 3))
  expect_equal(round(richnessICE(a, k = 10), 1), 5.6)
})
