# Helpers for testthat

# Set the seed for just a code block
# https://stackoverflow.com/questions/56191862/where-do-i-specify-random-seed-for-tests-in-r-package
with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  # Set sample.kind = "Rounding" to reproduce the old sampling
  # Keep the results the same for R versions prior to 3.6
  # Suppress warning "non-uniform 'Rounding' sampler used"
  if (getRversion() >= "3.6") {
    suppressWarnings(set.seed(seed, sample.kind = "Rounding"))
  } else {
    set.seed(seed)
  }
  eval.parent(code)
}
