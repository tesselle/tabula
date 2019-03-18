context("Date")
options("verbose" = FALSE)

test_that("Date model", {
  zuni <- as(zuni, "CountMatrix")
  dates <- list(
    LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
    LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
    LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
    LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
  )
  # Model the event and accumulation date for each assemblage.
  model <- dateEvent(zuni, dates, cutoff = 90,
                     jackknife = TRUE, bootstrap = TRUE)
  expect_s4_class(model, "DateModel")
  expect_is(plotDate(model, select = 1), "ggplot")

  expect_error(dateEvent(zuni, dates, cutoff = 10), "below 50%")
  expect_error(dateEvent(zuni, unlist(dates)), "list of dates is expected")

  dates <- list(XXX = 1)
  expect_error(dateEvent(zuni, dates), "do not match")
})
