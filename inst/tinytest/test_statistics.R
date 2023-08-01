# Compute thresholds ===========================================================
data("cantabria")

expect_equal_to_reference(eppm(cantabria), file = "_snaps/eppm.rds")
expect_equal_to_reference(pvi(cantabria), file = "_snaps/pvi.rds")

# Binomial coefficient =========================================================
expect_equal(combination(4, 3), 4)
# Ramanujan factorial approx.
expect_equal(combination(171, 3), 818816.247275706)
expect_error(combination(3, "a"))

options("tabula.verbose" = TRUE)
expect_message(combination(171, 3), "Ramanujan approximation of x!")
