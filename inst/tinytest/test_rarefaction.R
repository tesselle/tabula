# Data from Magurran 1988, p. 128-129
trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                        1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
               nrow = 2, byrow = TRUE,
               dimnames = list(c(1, 2), NULL))

rare <- rarefaction(trap, sample = 13)
expect_equal_to_reference(rare, file = "_snaps/rarefaction.rds")

# skip_if_not_installed("vdiffr")
# gg_idx_rarefaction <- autoplot(rare)
# vdiffr::expect_doppelganger("idx_rarefaction", gg_idx_rarefaction)
