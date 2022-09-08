birds <- matrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9),
  nrow = 2, byrow = TRUE
)
rownames(birds) <- c("unmanaged", "managed")

test_that("Similiraty measure (count data)", {
  method <- c("bray", "jaccard", "morisita", "sorenson")

  for (i in 1:length(method)) {
    expect_snapshot(similarity(birds, method = method[i]))
  }
})
test_that("Plot Similarity", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  test_sim <- similarity(cantabria, method = "brainerd")

  gg_mtx_sim <- plot_heatmap(test_sim, upper = TRUE)
  vdiffr::expect_doppelganger("mtx_sim", gg_mtx_sim)

  gg_spot_ring_sim <- plot_spot(test_sim, type = "ring")
  vdiffr::expect_doppelganger("spot_ring_sim", gg_spot_ring_sim)

  gg_spot_plain_sim <- plot_spot(test_sim, type = "plain")
  vdiffr::expect_doppelganger("spot_plain_sim", gg_spot_plain_sim)
})
test_that("Plot Co-Occurrence", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  test_occ <- occurrence(cantabria)

  gg_mtx_occ <- plot_heatmap(test_occ, upper = TRUE)
  vdiffr::expect_doppelganger("mtx_occ", gg_mtx_occ)

  gg_spot_occ <- plot_spot(test_occ, upper = FALSE)
  vdiffr::expect_doppelganger("spot_occ", gg_spot_occ)
})

# Indices ======================================================================
test_that("Jaccard index - character", {
  x <- c("horse", "dog", "cat", "cow")
  y <- c("sheep", "cat", "bird", "bull")

  expect_snapshot(index_jaccard(x, y)) # 0.14
})
test_that("Jaccard index - numeric", {
  # Magurran 1988, p. 165
  expect_snapshot(index_jaccard(birds[1, ], birds[2, ])) # 0.46
})
test_that("Soreson index", {
  # Magurran 1988, p. 165
  expect_snapshot(index_sorenson(birds[1, ], birds[2, ])) # 0.63
})
test_that("Bray index", {
  # Magurran 1988, p. 165
  expect_snapshot(index_bray(birds[1, ], birds[2, ])) # 0.44
})
test_that("Morisita-Horn", {
  # Magurran 1988, p. 167
  expect_snapshot(index_morisita(birds[1, ], birds[2, ])) # 0.8133
})
test_that("Brainerd-Robinson", {
  x <- c(16, 9, 3, 0, 1)
  y <- c(13, 3, 2, 0, 0)
  expect_snapshot(index_brainerd(x, y)) # 164
})
test_that("Binomial co-occurrence", {
  x <- c(16, 9, 3, 0, 1)
  y <- c(13, 3, 2, 0, 0)
  expect_snapshot(index_binomial(x, y)) # 0.54
})
