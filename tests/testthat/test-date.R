context("Date & Time")

test_that("Mean Ceramic Date", {
  # Load dataset
  skip_if_not_installed("folio")
  data("zuni", package = "folio")

  ## Set dates
  zuni_dates <- list(
    LINO = c(600, 875), KIAT = c(850, 950), RED = c(900, 1050),
    GALL = c(1025, 1125), ESC = c(1050, 1150), PUBW = c(1050, 1150),
    RES = c(1000, 1200), TULA = c(1175, 1300), PINE = c(1275, 1350),
    PUBR = c(1000, 1200), WING = c(1100, 1200), WIPO = c(1125, 1225),
    SJ = c(1200, 1300), LSJ = c(1250, 1300), SPR = c(1250, 1300),
    PINER = c(1275, 1325), HESH = c(1275, 1450), KWAK = c(1275, 1450)
  )
  ## Calculate mid-point
  zuni_mid_dates <- vapply(X = zuni_dates, FUN = mean, FUN.VALUE = numeric(1))

  keep_sites <- c("CS11", "CS12", "CS144", "CS195", "CS40", "LZ0219", "LZ0280",
                  "LZ0367", "LZ0508", "LZ0560", "LZ1076", "LZ1087")
  zuni2 <- zuni[rownames(zuni) %in% keep_sites, ]
  zuni2 <- arkhe::as_count(zuni2)

  dt <- date_mcd(zuni2, zuni_mid_dates)
  expected <- c(943, 1205, 1187, 1150, 782, 1148, 1156,
                1248, 1248, 1262, 1250, 1249)
  expect_equal(round(dt@mcd_values, 0), expected, check.attributes = FALSE)
})
test_that("Refine Date Model", {
  # Load dataset
  skip_if_not_installed("folio")
  data("zuni", package = "folio")

  dates <- c(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )
  counts <- arkhe::as_count(zuni)
  model <- date_event(counts, dates, cutoff = 90)
  event <- predict_event(model)

  # Jackknife
  refined_jack <- jackknife(model)
  expect_s3_class(refined_jack, "data.frame")

  # Jackknife
  refined_boot <- bootstrap(model)
  expect_s3_class(refined_boot, "data.frame")

  # Errors
  expect_error(date_event(counts, dates, cutoff = 10), "below 50%")
  expect_error(plot_date(event, select = "X"), "Wrong selection")

  skip_if_not_installed("vdiffr")
  # Event plot
  for (i in c(TRUE, FALSE)) {
    gg_date_act <- plot_date(event, type = "activity", event = i,
                             select = "LZ1105")
    vdiffr::expect_doppelganger(paste0("date_activity_event-", i), gg_date_act)
  }

  # Activity plot
  gg_date_tempo <- plot_date(event, type = "tempo", select = "LZ1105")
  vdiffr::expect_doppelganger("date_tempo", gg_date_tempo)
})
