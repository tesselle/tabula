context("Date & Time")
options("verbose" = FALSE)

test_that("Date plot", {
  count_zuni <- as(zuni, "CountMatrix")
  set_dates(count_zuni) <- list(value = c(LZ0569 = 1097, LZ0279 = 1119),
                               error = c(LZ0569 = 30, LZ0279 = 30))

  gg_date <- plot_date(count_zuni, select = c(1,2))
  vdiffr::expect_doppelganger("date", gg_date)
})
test_that("Date model", {
  count_zuni <- as(zuni, "CountMatrix")
  set_dates(count_zuni) <- c(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )
  model <- date_event(count_zuni, cutoff = 90)
  expect_s4_class(model, "DateModel")

  for (i in c(TRUE, FALSE)) {
    gg_date_act <- plot_date(model, type = "activity", event = i, select = 1)
    vdiffr::expect_doppelganger(paste0("date_activity_event-", i), gg_date_act)
  }
  gg_date_tempo <- plot_date(model, type = "tempo", select = 1)
  vdiffr::expect_doppelganger("date_tempo", gg_date_tempo)

  # Errors
  expect_error(date_event(count_zuni, cutoff = 10), "Cutoff value is below 50%")
  set_dates(count_zuni) <- NULL
  expect_error(date_event(count_zuni), "No dates were found!")
})
test_that("Time plot", {
  # Keep only decoration types that have a maximum frequency of at least 50
  keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
  count_merzbach <- as(merzbach[, keep], "CountMatrix")
  # Use the row names as time coordinates (roman numerals)
  set_dates(count_merzbach) <- rownames(merzbach)
  # Plot time vs abundance
  for (i in c(TRUE, FALSE)) {
    gg_time_facet <- plot_time(count_merzbach, facet = i)
    vdiffr::expect_doppelganger(paste0("time_facet-", i), gg_time_facet)
  }
  # Plot time vs abundance and highlight selection
  for (i in c(TRUE, FALSE)) {
    gg_time_roll <- plot_time(count_merzbach, highlight = "FIT", roll = i)
    vdiffr::expect_doppelganger(paste0("time_FIT_roll-", i), gg_time_roll)
  }

  # Errors
  set_dates(count_merzbach) <- NULL
  expect_error(plot_time(count_merzbach), "Time coordinates are missing!")
})
