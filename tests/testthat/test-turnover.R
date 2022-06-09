# Turnover measure =============================================================
test_that("Turnover measures", {
  incid <- matrix(c(2, 2, 3, 0, 0, 0,
                    4, 3, 5, 6, 1, 2,
                    0, 0, 6, 0, 3, 0,
                    0, 0, 0, 8, 8, 9,
                    0, 0, 0, 0, 5, 4,
                    0, 0, 0, 3, 0, 7),
                  ncol = 6, nrow = 6)

  method <- c("whittaker", "cody", "routledge1", "routledge2", "routledge3",
              "wilson")
  expected <- c(1.00, 3.00, 0.29, 0.56, 1.75, 1.00)

  for (i in 1:length(method)) {
    index <- turnover(incid, method = method[i])
    expect_equal(round(index, 2), expected[i])
  }
})

# Indices ======================================================================
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

test_that("Whittaker index", {
  # Magurran 1988, p. 162
  expect_equal(index_whittaker(trees), 1)
})
test_that("Cody index", {
  # Magurran 1988, p. 162
  expect_equal(index_cody(trees), 3)
})
test_that("Routledge 'R' index", {
  # Magurran 1988, p. 163
  expect_equal(round(index_routledge1(trees), 4), 0.2857)
})
test_that("Routledge 'I' index", {
  # Magurran 1988, p. 163
  expect_equal(round(index_routledge2(trees), 4), 0.5595)
})
test_that("Routledge 'E' index", {
  # Magurran 1988, p. 164
  expect_equal(round(index_routledge3(trees), 3), 1.750)
})
test_that("Wilson index", {
  # Magurran 1988, p. 164
  expect_equal(index_wilson(trees), 1)
})
