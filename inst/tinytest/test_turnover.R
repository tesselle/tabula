# Turnover measure =============================================================
incid <- matrix(c(2, 2, 3, 0, 0, 0,
                  4, 3, 5, 6, 1, 2,
                  0, 0, 6, 0, 3, 0,
                  0, 0, 0, 8, 8, 9,
                  0, 0, 0, 0, 5, 4,
                  0, 0, 0, 3, 0, 7),
                ncol = 6, nrow = 6)

method <- c("whittaker", "cody", "routledge1", "routledge2", "routledge3", "wilson")
expected <- c(1.00, 3.00, 0.29, 0.56, 1.75, 1.00)

for (i in 1:length(method)) {
  index <- turnover(incid, method = method[i])
  expect_equal(round(index, 2), expected[i])
}
