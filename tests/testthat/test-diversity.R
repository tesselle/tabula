test_that("Heterogeneity", {
  data("cantabria")

  method <- c("berger", "brillouin", "mcintosh", "simpson", "shannon")
  for (i in method) {
    index <- heterogeneity(cantabria, method = i)
    expect_length(index, nrow(cantabria))
    expect_equal(get_method(index), i)
  }

  boot <- with_seed(12345, bootstrap(index, n = 30))
  expect_snapshot(boot)

  skip("Remove skip() when arkhe is released")
  jack <- jackknife(index)
  expect_snapshot(jack)
})
test_that("Evenness", {
  data("cantabria")

  method <- c("brillouin", "mcintosh", "simpson", "shannon")
  for (i in method) {
    index <- evenness(cantabria, method = i)
    expect_length(index, nrow(cantabria))
    expect_equal(get_method(index), i)
  }

  boot <- with_seed(12345, bootstrap(index, n = 30))
  expect_snapshot(boot)

  skip("Remove skip() when arkhe is released")
  jack <- jackknife(index)
  expect_snapshot(jack)
})
test_that("Plot", {
  data("cantabria")

  skip_if_not_installed("vdiffr")
  idx_heterogeneity <- with_seed(12345, {
    idx_heterogeneity <- heterogeneity(cantabria, method = "shannon")
    sim_heterogeneity <- simulate(idx_heterogeneity, n = 100)
  })
  gg_heterogeneity <- autoplot(sim_heterogeneity)
  vdiffr::expect_doppelganger("idx_heterogeneity", gg_heterogeneity)
})
# Indices ======================================================================
test_that("Berger-Parker dominance", {
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_snapshot(index_berger(n), cran = TRUE) # 1 / 3.49
})
test_that("Brillouin diversity", {
  # Magurran 1988, p. 150
  n <- c(17, 15, 11, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1)
  expect_snapshot(index_brillouin(n, evenness = FALSE), cran = TRUE)
  expect_snapshot(index_brillouin(n, evenness = TRUE), cran = TRUE) # 0.827
})
test_that("McIntosh dominance", {
  # Magurran 1988, p. 154
  n <- c(254, 153, 90, 69, 68, 58, 51, 45, 40, 39, 25, 23, 19, 18, 16, 14, 14,
         11, 11, 11, 11, 10, 6, 6, 6, 6, 5, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1)
  expect_snapshot(index_mcintosh(n, evenness = FALSE), cran = TRUE)
  expect_snapshot(index_mcintosh(n, evenness = TRUE), cran = TRUE)
})
test_that("Shannon diversity", {
  # Magurran 1988, p. 145
  n <- c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1)
  expect_snapshot(index_shannon(n, evenness = FALSE), cran = TRUE) # 2.404
  expect_snapshot(index_shannon(n, evenness = TRUE), cran = TRUE) # 0.8025
  expect_snapshot(variance_shannon(n)) # 0.00502

  # Data from Magurran 1988, p. 145-149
  birds <- matrix(
    data = c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3,
             3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 0, 0,
             30, 30, 3, 65, 20, 11, 0, 4, 2, 14,
             0, 3, 9, 0, 0, 5, 0, 0, 0, 0, 1, 1),
    nrow = 2, byrow = TRUE, dimnames = list(c("oakwood", "spruce"), NULL))

  expect_equal(round(test_diversity(birds)[1, 1], 5), 0.00046)
})
test_that("Simpson dominance", {
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_snapshot(index_simpson(n), cran = TRUE) # 1 / 8.50
})
