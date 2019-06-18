context("Similarity")

birds_unmanaged <- c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3,
                     13.0, 14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9,
                     4.3, 1.4, 2.9)
birds_managed <- c(0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7,
                   2.9, 0, 0, 2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9)
birds <- rbind(birds_unmanaged, birds_managed)

test_that("Similiraty measure (count data)", {
  count <- as(birds * 10, "CountMatrix")
  method <- c("brainerd", "bray", "jaccard", "morisita", "sorenson")

  for (i in 1:length(method)) {
    index <- similarity(count, method = method[i])
    expect_is(index, "matrix")
    expect_equal(length(index), nrow(count)^2, info = method[i])
  }

  expect_is(similarity(count, method = "binomial"), "matrix")
  expect_equal(length(similarity(count, method = "binomial")), ncol(count)^2)
})
test_that("Similiraty measure (frequency data)", {
  freq <- as(birds, "FrequencyMatrix")
  method <- c("bray", "jaccard", "morisita", "sorenson")
  expect_error(similarity(freq, method = "bray"))
})
test_that("Similiraty measure (presence/absence data)", {
  bin <- as(birds, "IncidenceMatrix")
  method1 <- c("jaccard", "sorenson")
  method2 <- c("bray", "morisita")

  for (i in 1:length(method1)) {
    index <- similarity(bin, method = method1[i])
    expect_is(index, "matrix")
    expect_equal(length(index), nrow(bin) * 2)
    expect_error(similarity(bin, method = method2[i]), info = method[i])
  }
})

# Indices ======================================================================
test_that("Jaccard index", {
  # Magurran 1988, p. 165
  expect_equal(round(similarityJaccard(birds_unmanaged, birds_managed), 2), 0.46)

  expect_error(similarityJaccard(1:10, 1:5))
})
test_that("Soreson index", {
  # Magurran 1988, p. 165
  expect_equal(round(similaritySorenson(birds_unmanaged, birds_managed), 2), 0.63)

  expect_error(similaritySorenson(1:10, 1:5))
})
test_that("Bray index", {
  # Magurran 1988, p. 165
  expect_equal(round(similarityBray(birds_unmanaged, birds_managed), 2), 0.44)

  expect_error(similarityBray(1:10, 1:5))
  expect_error(similarityBray(1:26, LETTERS))
})
test_that("Morisita-Horn", {
  # Magurran 1988, p. 167
  expect_equal(round(similarityMorisita(birds_unmanaged, birds_managed), 4), 0.8134) # 0.8133

  expect_error(similarityMorisita(1:10, 1:5))
  expect_error(similarityMorisita(1:26, LETTERS))
})
test_that("Brainerd-Robinson", {
  x <- c(16, 9, 3, 0, 1)
  y <- c(13, 3, 2, 0, 0)
  expect_equal(round(similarityBrainerd(x, y), 0), 164)

  expect_error(similarityBrainerd(1:10, 1:5))
  expect_error(similarityBrainerd(1:26, LETTERS))
})
test_that("Binomial co-occurrence", {
  x <- c(16, 9, 3, 0, 1)
  y <- c(13, 3, 2, 0, 0)
  expect_equal(round(similarityBinomial(x, y), 2), 0.54)

  expect_error(similarityBinomial(1:10, 1:5))
  expect_error(similarityBinomial(1:26, LETTERS))
})
