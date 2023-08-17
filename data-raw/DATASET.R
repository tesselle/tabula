# PREPARE DATASETS

# The Madgalenian dataset from Conkey 1980, Kintigh 1989
cantabria <- read.csv("data-raw/cantabria.csv", header = TRUE, row.names = 1,
                      sep = ",", dec = ".")
usethis::use_data(cantabria, overwrite = FALSE)

# The Pueblo IV dataset from Huntley 2004, 2008
pueblo <- read.csv("data-raw/pueblo.csv", header = TRUE, row.names = 1,
                   sep = ",", dec = ".")
usethis::use_data(pueblo, overwrite = FALSE)

# The birds dataset from Magurran 1988, p. 166
aves <- matrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("unmanaged", "managed"),
    c("Great-crested grebe", "Mallard", "Mute swan", "Moorhen", "Coot", "Common sandpiper",
      "Kingfisher", "Sandmartin", "Dipper", "Sedge warbler", "Pied wagtail", "Grey wagtail",
      "Yellow wagtail", "Reed bunting", "Heron", "Curlew", "Lapwing", "Redshank",
      "Nuthatch", "Tree-creeper", "Whinchat", "Blackcap", "Garden warbler",
      "Whitethroat", "Lesser whitethroat", "Spotted fly-catcher")
  )
)
aves <- as.data.frame(aves)
usethis::use_data(aves, overwrite = FALSE)

# The trees dataset from Magurran 1988, p. 162
woodland <- matrix(
  c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, TRUE, FALSE, TRUE, FALSE,
    FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
    FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
  nrow = 6,
  ncol = 6,
  byrow = FALSE,
  dimnames = list(
    c("1", "2", "3", "4", "5", "6"),
    c("Birch", "Oak", "Rowan", "Beech", "Hazel", "Holly")
  )
)
woodland <- as.data.frame(woodland)
usethis::use_data(woodland, overwrite = FALSE)
