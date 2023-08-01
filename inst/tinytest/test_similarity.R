birds <- matrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9),
  nrow = 2, byrow = TRUE
)
rownames(birds) <- c("unmanaged", "managed")

# Similiraty measure (count data) ==============================================
# method <- c("bray", "jaccard", "morisita", "sorenson")
# expect_equal(round(similarity(birds, method = "bray"), 3), 0.444)

# Plot Similarity ==============================================================
# skip_if_not_installed("vdiffr")
# data("cantabria")
#
# test_sim <- similarity(cantabria, method = "brainerd")
#
# gg_mtx_sim <- plot_heatmap(test_sim, upper = TRUE)
# vdiffr::expect_doppelganger("mtx_sim", gg_mtx_sim)
#
# gg_spot_ring_sim <- plot_spot(test_sim, type = "ring")
# vdiffr::expect_doppelganger("spot_ring_sim", gg_spot_ring_sim)
#
# gg_spot_plain_sim <- plot_spot(test_sim, type = "plain")
# vdiffr::expect_doppelganger("spot_plain_sim", gg_spot_plain_sim)
