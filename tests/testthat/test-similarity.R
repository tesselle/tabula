birds <- matrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9),
  nrow = 2, byrow = TRUE
)
rownames(birds) <- c("unmanaged", "managed")

test_that("Similiraty measure (count data)", {
  method <- c("brainerd", "bray", "jaccard", "morisita", "sorenson")

  for (i in 1:length(method)) {
    expect_snapshot(similarity(birds, method = method[i]))
  }
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
