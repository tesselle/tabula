Sys.setenv(LANGUAGE = "en") # Force locale

source("helpers.R")
data("cantabria")

# Richness =====================================================================
method <- c("margalef", "menhinick", "observed")
for (i in method) {
  index <- richness(cantabria, method = i)
  expect_length(index, nrow(cantabria))
  expect_equal(get_method(index), i)
}

# Composition ==================================================================
# Data from Magurran 1988, p. 128-129
trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                        1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
               nrow = 2, byrow = TRUE)

method <- c("chao2", "ice")
for (i in method) {
  index <- composition(trap, method = i)
  expect_length(index, 1)
  expect_equal(get_method(index), i)
}
