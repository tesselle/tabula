# Plot Co-Occurrence ===========================================================
# skip_if_not_installed("vdiffr")
# data("cantabria")
#
# test_occ <- occurrence(cantabria)
#
# gg_mtx_occ <- plot_heatmap(test_occ, upper = TRUE)
# vdiffr::expect_doppelganger("mtx_occ", gg_mtx_occ)
#
# gg_spot_occ <- plot_spot(test_occ, upper = FALSE)
# vdiffr::expect_doppelganger("spot_occ", gg_spot_occ)
