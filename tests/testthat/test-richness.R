# Richness =====================================================================
test_that("Richness", {
  data("cantabria")

  method <- c("margalef", "menhinick", "count")
  for (i in method) {
    index <- richness(cantabria, method = i)
    expect_length(index, nrow(cantabria))
    expect_equal(get_method(index), i)
  }

  boot <- with_seed(12345, bootstrap(index, n = 30))
  expect_snapshot(boot)

  skip("Remove skip() when arkhe is released")
  jack <- jackknife(index)
  expect_snapshot(jack)
})
test_that("Composition", {
  # Data from Magurran 1988, p. 128-129
  trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                          1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                 nrow = 2, byrow = TRUE)

  method <- c("chao2", "ice")
  for (i in method) {
    index <- composition(trap, method = i)
    expect_length(index, 1)
    expect_equal(get_method(index), i)
  }
})
test_that("Plot", {
  data("cantabria")

  skip_if_not_installed("vdiffr")
  idx_richness <- with_seed(12345, {
    idx_richness <- richness(cantabria, method = "count")
    sim_richness <- simulate(idx_richness, n = 100)
  })
  gg_richness <- autoplot(sim_richness)
  vdiffr::expect_doppelganger("idx_richness", gg_richness)
})
# Indices ======================================================================
test_that("Margalef richness", {
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_snapshot(index_margalef(n), cran = TRUE) # 5.47
})
test_that("Menhinick richness", {
  # Magurran 1988, p. 128
  n <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  expect_snapshot(index_menhinick(n), cran = TRUE) # 1.88
})

# Data from Chao & Chiu (2016)
x <- c(1:21, 23, 25, 27, 28, 30, 32, 34:37, 41, 45, 46, 49, 52, 89, 110, 123, 140)
edge <- rep(x = x, times = c(113, 50, 39, 29, 15, 11, 13, 5, 6, 6, 3, 4, 3, 5,
                             2, 5, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1,
                             0, 0, 2, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0))
interior <- rep(x = x, times = c(129, 49, 42, 32, 19, 17, 7, 9, 7, 7, 6, 3, 3, 3,
                                 4, 4, 2, 2, 3, 4, 6, 2, 1, 2, 1, 1, 1,
                                 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1))

test_that("Chao1-type estimators", {
  expect_snapshot(index_chao1(edge, unbiased = FALSE), cran = TRUE) # 461.625
  expect_snapshot(index_chao1(edge, unbiased = TRUE), cran = TRUE) # 458.016
  expect_snapshot(index_chao1(edge, unbiased = FALSE, improved = TRUE), cran = TRUE) # 488.313

  expect_snapshot(index_chao1(interior, unbiased = FALSE), cran = TRUE) # 540.728
  expect_snapshot(index_chao1(interior, unbiased = TRUE), cran = TRUE) # 536.044
  expect_snapshot(index_chao1(interior, unbiased = FALSE, improved = TRUE), cran = TRUE) # 572.505
})
test_that("ACE-type estimators", {
  expect_snapshot(index_ace(edge, k = 10), cran = TRUE) # 443.684
  expect_snapshot(index_ace(interior, k = 10), cran = TRUE) # 498.834
})
test_that("Chao2-type estimators", {
  a <- matrix(
    c(0, 1, 1, 1,
      1, 0, 0, 1,
      0, 0, 1, 1),
    nrow = 3, ncol = 4, byrow = TRUE
  )
  b <- matrix(
    c(0, 1, 1, 1,
      1, 0, 0, 1,
      0, 0, 0, 1),
    nrow = 3, ncol = 4, byrow = TRUE
  )
  c <- matrix(
    c(0, 1, 1, 1,
      1, 0, 0, 1,
      0, 0, 1, 1,
      1, 0, 1, 1),
    nrow = 4, ncol = 4, byrow = TRUE
  )
  expect_snapshot(index_chao2(a, unbiased = TRUE), cran = TRUE) # 4.33
  expect_snapshot(index_chao2(a, unbiased = FALSE), cran = TRUE) # 5.33
  expect_snapshot(index_chao2(b, unbiased = FALSE), cran = TRUE) # 6
  expect_snapshot(index_chao2(c, unbiased = TRUE, improved = TRUE), cran = TRUE) # 4.05
  expect_snapshot(index_chao2(c, unbiased = FALSE, improved = TRUE), cran = TRUE) # 4.43
})
test_that("ICE-type estimators", {
  a <- matrix(
    c(0, 1, 1, 1,
      1, 0, 0, 1,
      0, 0, 1, 1),
    nrow = 3, ncol = 4, byrow = TRUE
  )
  expect_snapshot(index_ice(a, k = 10), cran = TRUE) # 5.6
})
