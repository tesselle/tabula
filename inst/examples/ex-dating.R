## Mean Ceramic Date
## Coerce the zuni dataset to an abundance (count) matrix
zuni_counts <- as_count(zuni)
## Set the start and end dates for each ceramic type
zuni_dates <- list(
  LINO = c(600, 875), KIAT = c(850, 950), RED = c(900, 1050),
  GALL = c(1025, 1125), ESC = c(1050, 1150), PUBW = c(1050, 1150),
  RES = c(1000, 1200), TULA = c(1175, 1300), PINE = c(1275, 1350),
  PUBR = c(1000, 1200), WING = c(1100, 1200), WIPO = c(1125, 1225),
  SJ = c(1200, 1300), LSJ = c(1250, 1300), SPR = c(1250, 1300),
  PINER = c(1275, 1325), HESH = c(1275, 1450), KWAK = c(1275, 1450)
)
## Calculate date midpoints
zuni_mid <- vapply(X = zuni_dates, FUN = mean, FUN.VALUE = numeric(1))

## Calculate MCD
## (we use a bootstrapping procedure to estimate the confidence interval)
zuni_mcd <- date_mcd(zuni_counts, dates = zuni_mid)
head(zuni_mcd)

## Event and accumulation dates (Bellanger et al.)
## See the vignette:
\donttest{
utils::vignette("dating", package = "tabula")
}
