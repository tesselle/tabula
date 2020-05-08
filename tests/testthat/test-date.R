context("Date & Time")

test_that("Mean Ceramic Date", {
  zuni_dates <- list(
    LINO = c(600, 875), KIAT = c(850, 950), RED = c(900, 1050),
    GALL = c(1025, 1125), ESC = c(1050, 1150), PUBW = c(1050, 1150),
    RES = c(1000, 1200), TULA = c(1175, 1300), PINE = c(1275, 1350),
    PUBR = c(1000, 1200), WING = c(1100, 1200), WIPO = c(1125, 1225),
    SJ = c(1200, 1300), LSJ = c(1250, 1300), SPR = c(1250, 1300),
    PINER = c(1275, 1325), HESH = c(1275, 1450), KWAK = c(1275, 1450)
  )
  # Calculate mid-point
  zuni_mid_dates <- vapply(X = zuni_dates, FUN = mean, FUN.VALUE = numeric(1))

  keep_sites <- c("CS11", "CS12", "CS144", "CS195", "CS40", "LZ0219", "LZ0280",
                  "LZ0367", "LZ0508", "LZ0560", "LZ1076", "LZ1087")
  zuni2 <- zuni[rownames(zuni) %in% keep_sites, ]
  zuni2 <- as(zuni2, "CountMatrix")

  dt <- date_mcd(zuni2, zuni_mid_dates)
  expect_equal(round(dt[, 1]), c(943, 1205, 1187, 1150, 782, 1148, 1156,
                                 1248, 1248, 1262, 1250, 1249))
})

test_that("Time plot", {
  # Keep only decoration types that have a maximum frequency of at least 50
  keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
  counts <- as(merzbach[, keep], "CountMatrix")
  # Use the row names as time coordinates (roman numerals)
  dates <- as.numeric(as.roman(rownames(counts)))
  # Plot time vs abundance
  for (i in c(TRUE, FALSE)) {
    gg_time_facet <- plot_time(counts, dates, facet = i)
    vdiffr::expect_doppelganger(paste0("time_facet-", i), gg_time_facet)
  }
  # Plot time vs abundance and highlight selection
  for (i in c(TRUE, FALSE)) {
    gg_time_roll <- plot_time(counts, dates, highlight = "FIT", roll = i)
    vdiffr::expect_doppelganger(paste0("time_FIT_roll-", i), gg_time_roll)
  }

  # Errors
  expect_error(plot_time(counts, dates, highlight = "FIT",
                         roll = TRUE, window = 2), "must be an odd integer")
})
test_that("Date plot", {
  count_zuni <- as(zuni, "CountMatrix")
  expect_warning(set_dates(count_zuni) <- list(value = c(X = 1097),
                                               error = c(X = 1119)))

  set_dates(count_zuni) <- list(value = c(LZ0569 = 1097, LZ0279 = 1119),
                                error = c(LZ0569 = 30, LZ0279 = 30))

  expect_s3_class(plot_date(count_zuni, select = NULL), "ggplot")
  expect_s3_class(plot_date(count_zuni, select = "LZ0569"), "ggplot")
  gg_date <- plot_date(count_zuni, select = c(1, 2))
  vdiffr::expect_doppelganger("date", gg_date)

  expect_error(plot_date(count_zuni, select = "X"), "Wrong selection")
})
test_that("Date model", {
  dates <- c(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )
  counts <- zuni[rownames(zuni) %in% names(dates), ]
  counts <- arkhe::as_count(counts)

  model <- date_event(counts, dates, cutoff = 90)
  expect_s4_class(model, "DateModel")
  event <- predict_event(model, counts)

  for (i in c(TRUE, FALSE)) {
    gg_date_act <- plot_date(event, type = "activity", event = i,
                             select = c(1, 2))
    vdiffr::expect_doppelganger(paste0("date_activity_event-", i), gg_date_act)
  }
  gg_date_tempo <- plot_date(event, type = "tempo", select = 1)
  vdiffr::expect_doppelganger("date_tempo", gg_date_tempo)

  # Errors
  expect_error(date_event(counts, dates, cutoff = 10),
               "Cutoff value is below 50%")
  expect_error(plot_date(event, select = "X"), "Wrong selection")
})
test_that("Refine date model", {
  dates <- c(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )
  counts <- zuni[rownames(zuni) %in% names(dates), ]
  counts <- arkhe::as_count(counts)
  model <- date_event(counts, dates, cutoff = 90)

  # Jackknife
  refined_jack <- refine_event(model, method = "jackknife")
  expect_s3_class(refined_jack, "data.frame")

  # Jackknife
  refined_boot <- refine_event(model, method = "bootstrap")
  expect_s3_class(refined_boot, "data.frame")
})
