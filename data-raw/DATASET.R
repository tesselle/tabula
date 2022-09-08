# PREPARE DATASETS

# The Madgalenian dataset from Conkey 1980, Kintigh 1989
cantabria <- read.csv("data-raw/cantabria.csv", header = TRUE, row.names = 1,
                      sep = ",", dec = ".")
usethis::use_data(cantabria, overwrite = FALSE)
