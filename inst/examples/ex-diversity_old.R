## Shannon diversity index
## Data from Magurran 1988, p. 145-149
data("birds", package = "folio")

heterogeneity(birds, "shannon") # 2.40 2.06
evenness(birds, "shannon") # 0.80 0.78

## Brillouin diversity index
## Data from Magurran 1988, p. 150-151
moths <- matrix(
  data = c(17, 15, 11, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1),
  nrow = 1,
  byrow = TRUE
)

heterogeneity(moths, "brillouin") # 1.88
evenness(moths, "brillouin") # 0.83

## Simpson dominance index
## Data from Magurran 1988, p. 152-153
trees <- matrix(
  data = c(752, 276, 194, 126, 121, 97, 95, 83, 72, 44,
            39,  16,  15,  13,   9,  9,  9,  8,  7,  4,
             2,   2,   1,   1,   1),
  nrow = 1,
  byrow = TRUE
)

heterogeneity(trees, "simpson") # 1.19
evenness(trees, "simpson") # 0.21

## McIntosh dominance index
## Data from Magurran 1988, p. 154-155
invertebrates <- matrix(
  data = c(254, 153, 90, 69, 68, 58, 51, 45, 40, 39,
            25,  23, 19, 18, 16, 14, 14, 11, 11, 11,
            11,  10,  6,  6,  6,  6,  5,  3,  3,  3,
             3,   3,  1,  1,  1,  1,  1,  1),
  nrow = 1,
  byrow = TRUE
)

heterogeneity(invertebrates, "mcintosh") # 0.71
evenness(invertebrates, "mcintosh") # 0.82

## Berger-Parker dominance index
## Data from Magurran 1988, p. 156-157
fishes <- matrix(
  data = c(394, 3487, 275,  683,   22,   1,    0,   1,   6,   8,
             1,    1,   2, 1642, 5681, 196, 1348,  12,   0,   1,
            48,   21,   1,    5,    7,   3,   90, 320, 180,  46,
             2,    0,   0,    1,    0,   0,    2,   1,   5, 126,
            17,  115, 436,   27,    0,   0,    3,   1,   0,   0,
             1,    0,  32,    0,    0,   5,    0,   0,   0,   0,
            13,    9,   0,    0,    4),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(c("station 1", "station 2", "station 3",
                    "station 4", "station 5"), NULL)
)

heterogeneity(fishes, "berger") # 0.71 0.63 0.50 0.60 0.51
