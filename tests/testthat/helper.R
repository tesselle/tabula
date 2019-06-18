# Helpers for testthat

# Set the seed for just a code block
# https://stackoverflow.com/questions/56191862/where-do-i-specify-random-seed-for-tests-in-r-package
with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}
