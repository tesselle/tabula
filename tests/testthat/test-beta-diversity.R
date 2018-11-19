context("beta-diversity")
library(tabula)
options("verbose" = TRUE)

# Turnover measure =============================================================
test_that("Turnover measure (presence/absence data)", {
  trees <- matrix(
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      FALSE, FALSE, TRUE, FALSE, TRUE, FALSE,
      FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
      FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
    nrow = 6, ncol = 6, byrow = FALSE,
    dimnames = list(c("1", "2", "3", "4", "5", "6"),
                    c("Birch", "Oak", "Rowan", "Beech", "Hazel", "Holly"))
  )
  count <- as(trees, "IncidenceMatrix")
  method <- c("whittaker", "cody", "routledge1", "routledge2", "routledge3",
              "wilson")
  expected <- c(1.00, 3.00, 0.29, 0.56, 1.75, 1.00)
  for (i in 1:length(method)) {
    index <- turnover(count, method = method[i])
    expect_is(index, "numeric")
    expect_identical(round(index, 2), expected[i])
  }
})

# Similarity measure ===========================================================
birds <- rbind(
  `unmanaged` = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3,
                  13.0, 14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9,
                  4.3, 1.4, 2.9),
  `managed` = c(0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9,
                0, 0, 2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9)
)

test_that("Similiraty measure (count data)", {
  count <- as(birds * 10, "CountMatrix")
  method <- c("brainerd", "bray", "jaccard", "morisita", "sorenson")
  for (i in 1:length(method)) {
    index <- similarity(count, method = method[i])
    expect_is(index, "matrix")
    expect_equal(length(index), nrow(count) * 2)
  }
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
    expect_error(similarity(bin, method = method2[i]))
  }
})
