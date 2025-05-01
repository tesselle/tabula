Sys.setenv(LANGUAGE = "en") # Force locale

# Data from Magurran 1988, p. 128-129
trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                        1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
               nrow = 2, byrow = TRUE,
               dimnames = list(c(1, 2), NULL))

rare <- rarefaction(trap, sample = 13)
expect_equal_to_reference(rare, file = "_snaps/rarefaction.rds")

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  data("cantabria")
  rare <- rarefaction(cantabria, sample = 23, method = "baxter")
  plot_rarefaction <- function() plot(rare)
  expect_snapshot_plot(plot_rarefaction, "plot_rarefaction")
  plot_rarefaction_nolegend <- function() plot(rare, legend = NULL)
  expect_snapshot_plot(plot_rarefaction_nolegend, "plot_rarefaction_nolegend")
}
