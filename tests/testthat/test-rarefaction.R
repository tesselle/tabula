# Rarefaction ==================================================================
test_that("Rarefaction", {
  # Data from Magurran 1988, p. 128-129
  trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                          1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
                 nrow = 2, byrow = TRUE,
                 dimnames = list(c(1, 2), NULL))
  rare <- rarefaction(trap, sample = 13)

  expect_snapshot(rare)

  skip_if_not_installed("vdiffr")
  gg_idx_rarefaction <- autoplot(rare)
  vdiffr::expect_doppelganger("idx_rarefaction", gg_idx_rarefaction)
})

# Indices ======================================================================
test_that("Hurlbert rarefaction", {
  # Magurran 1988, p. 128
  n1 <- c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1)
  n2 <- c(1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0)
  expect_snapshot(index_hurlbert(n1, 13)) # 6.58
})
test_that("Baxter rarefaction", {
  # Baxter 2001, p. 720
  a <- c(2, 12, 7, 1, 0, 3, 12, 15, 0, 3, 1, 1, 12, 7, 3, 11, 3, 1, 7, 2, 4, 3,
         3, 1, 5, 1, 1, 1, 0, 2, 0, 1, 0, 1, 1, 1, 3, 0, 2, 1, 4, 5, 4, 5)
  expect_snapshot(index_baxter(a, sample = 5)) # 4.59
})
