context("Chao")
library(tabula)
options("verbose" = TRUE)

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
  expect_equal(round(chao1Richness(edge, unbiased = FALSE), 3), 461.625)
  expect_equal(round(chao1Richness(edge, unbiased = TRUE), 3), 458.016)
  expect_equal(round(chao1iRichness(edge, unbiased = FALSE), 3), 488.284) # 488.313

  expect_equal(round(chao1Richness(interior, unbiased = FALSE), 3), 540.728)
  expect_equal(round(chao1Richness(interior, unbiased = TRUE), 3), 536.044)
  expect_equal(round(chao1iRichness(interior, unbiased = FALSE), 3), 572.471) # 572.505
})
test_that("ACE-type estimators", {
  expect_equal(round(aceRichness(edge, k = 10), 3), 445.822) # 443.684
  expect_equal(round(aceRichness(interior, k = 10), 3), 501.045) # 498.834
})
test_that("Chao2-type estimators", {
  a <- t(matrix(c(0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1), 4, 3))
  expect_equal(round(chao2Richness(a, unbiased = FALSE), 2), 5.33)
  expect_equal(round(chao2Richness(a, unbiased = TRUE), 2), 4.33)
  expect_error(chao2iRichness(a, unbiased = FALSE))
})
test_that("ICE-type estimators", {
  a <- t(matrix(c(0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1), 4, 3))
  expect_equal(round(iceRichness(a, k = 10), 1), 5.6)
})
