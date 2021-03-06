test_that("Heterogeneity", {
  skip_if_not_installed("folio")
  data("chevelon", package = "folio")
  counts <- as_count(chevelon)

  method <- c("berger", "brillouin", "mcintosh", "shannon", "simpson")
  for (i in method) {
    index <- index_heterogeneity(counts, method = i)
    expect_length(index@values, nrow(counts))
    expect_equal(get_method(index), i)
  }
  expect_type(get_index(index), "closure")

  boot <- with_seed(12345, bootstrap_heterogeneity(counts, method = "shannon", n = 30))
  expect_snapshot(boot)

  jack <- jackknife_heterogeneity(counts, method = "shannon")
  expect_snapshot(jack)
})
test_that("Evenness", {
  skip_if_not_installed("folio")
  data("chevelon", package = "folio")
  counts <- as_count(chevelon)

  method <- c("brillouin", "mcintosh", "shannon", "simpson")
  for (i in method) {
    index <- index_evenness(counts, method = i)
    expect_length(index@values, nrow(counts))
    expect_equal(get_method(index), i)
  }
  expect_type(get_index(index), "closure")

  # boot <- with_seed(12345, bootstrap_evenness(counts, method = "shannon", n = 30))
  # expect_snapshot(boot)

  jack <- jackknife_evenness(counts, method = "shannon")
  expect_snapshot(jack)
})
test_that("Shannon diversity test", {
  # Data from Magurran 1988, p. 145-149
  birds <- CountMatrix(
    data = c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3,
             3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 0, 0,
             30, 30, 3, 65, 20, 11, 0, 4, 2, 14,
             0, 3, 9, 0, 0, 5, 0, 0, 0, 0, 1, 1),
    nrow = 2, byrow = TRUE, dimnames = list(c("oakwood", "spruce"), NULL))

  expect_equal(round(test_diversity(birds)[1, 1], 5), 0.00046)
})

# Indices ======================================================================
test_that("Shannon diversity", {
  expect_error(diversityShannon(LETTERS))
  expect_error(evennessShannon(LETTERS))
  expect_error(varianceShannon(LETTERS))

  # Magurran 1988, p. 38
  n <- c(235, 218, 192, 87, 20, 11, 11, 8, 7, 4, 3, 2, 2, 1, 1)
  expect_equal(round(diversityShannon(n), 2), 1.69)
  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(round(diversityShannon(n), 2), 2.61)
  expect_equal(round(evennessShannon(n), 2), 0.73) # 0.74
  # Magurran 1988, p. 145
  n1 <- c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1)
  expect_equal(round(diversityShannon(n1), 3), 2.408) # 2.404
  expect_equal(round(evennessShannon(n1), 4), 0.8038) # 0.8025
  expect_equal(round(varianceShannon(n1), 5), 0.00540) # 0.00502
  n2 <- c(65, 30, 30, 20, 14, 11, 9, 5, 4, 3, 3, 2, 1, 1)
  expect_equal(round(diversityShannon(n2), 3), 2.056)
  expect_equal(round(evennessShannon(n2), 4), 0.7791)
  expect_equal(round(varianceShannon(n2), 5), 0.00452) # 0.00427
})
test_that("Brillouin diversity", {
  expect_error(diversityBrillouin(LETTERS))
  expect_error(evennessBrillouin(LETTERS))

  # Magurran 1988, p. 38
  n <- c(235, 218, 192, 87, 20, 11, 11, 8, 7, 4, 3, 2, 2, 1, 1)
  expect_equal(round(diversityBrillouin(n), 2), 1.65)
  # Magurran 1988, p. 150
  n <- c(17, 15, 11, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1)
  expect_equal(round(diversityBrillouin(n), 3), 1.876)
  expect_equal(round(evennessBrillouin(n), 3), 0.828) # 0.827
})
test_that("Simpson dominance", {
  expect_error(dominanceSimpson(LETTERS))
  expect_error(evennessSimpson(LETTERS))

  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(round(dominanceSimpson(n), 3), 0.118) # 1 / 8.50
  # Magurran 1988, p. 152
  n <- c(752, 276, 194, 126, 121, 97, 95, 83, 72, 44, 39, 16, 15, 13, 9, 9, 9,
         8, 7, 4, 2, 2, 1, 1, 1)
  expect_equal(round(dominanceSimpson(n), 3), 0.187)
})
test_that("McIntosh dominance", {
  expect_error(dominanceMcintosh(LETTERS))
  expect_error(evennessMcintosh(LETTERS))

  # Magurran 1988, p. 154
  n <- c(254, 153, 90, 69, 68, 58, 51, 45, 40, 39, 25, 23, 19, 18, 16, 14, 14,
         11, 11, 11, 11, 10, 6, 6, 6, 6, 5, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1)
  expect_equal(round(dominanceMcintosh(n), 4), 0.7066)
  expect_equal(round(evennessMcintosh(n), 4), 0.8180)
})
test_that("Berger-Parker dominance", {
  expect_error(dominanceBerger(LETTERS))

  # Magurran 1988, p. 63
  n <- c(1, 3, 2, 1, 4, 5, 1, 1, 18, 1, 2, 63, 2, 1, 1, 1, 16, 15, 60, 1, 1, 8,
         16, 127, 9, 18, 3, 4, 3, 11, 6, 7, 8, 63, 17)
  expect_equal(dominanceBerger(n), 0.254) # 1 / 3.49
  # Magurran 1988, p. 156
  n <- c(394, 3487, 275, 683, 22, 1, 0, 1, 6, 8, 1, 1, 2)
  expect_equal(round(dominanceBerger(n), 3), 0.714)
})
