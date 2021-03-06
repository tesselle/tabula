test_that("Mean Ceramic Date", {
  skip_if_not_installed("folio")
  data("zuni", package = "folio")
  counts <- arkhe::as_count(zuni)

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

  ## MCD
  dt <- date_mcd(counts, dates = zuni_mid_dates)
  expect_snapshot(dt)
  expect_error(date_mcd(counts, dates = zuni_mid_dates[1:3]))

  ## Bootstrap
  boot <- with_seed(12345, bootstrap_mcd(counts, dates = zuni_mid_dates, n = 30))
  expect_snapshot(boot)
})
