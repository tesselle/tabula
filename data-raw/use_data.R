# The Madgalenian dataset from Conkey 1980, Kintigh 1989
altamira <- read.csv("data-raw/altamira.csv", header = TRUE, row.names = 1,
                        sep = ",", dec = ".") %>% as.matrix()
usethis::use_data(altamira, overwrite = FALSE)
# The Compiegne market dataset from Desachy 2004
compiegne <- read.csv("data-raw/compiegne.csv", header = TRUE, row.names = 1,
                      sep = ",", dec = ".") %>% as.matrix()
usethis::use_data(compiegne, overwrite = FALSE)
# The Boves dataset from Desachy 2004
boves <- read.csv("data-raw/boves.csv", header = TRUE, row.names = 1,
                  sep = ",", dec = ".") %>% as.matrix()
usethis::use_data(boves, overwrite = FALSE)
# The Merzbach dataset from Crema et al. 2016
merzbach <- read.csv("data-raw/merzbach.csv", header = TRUE, row.names = 1,
                     sep = ",", dec = ".") %>% as.matrix()
usethis::use_data(merzbach, overwrite = FALSE)
# The Mississippi dataset from Lippo 2015
mississippi <- read.csv("data-raw/mississippi.csv", header = TRUE, row.names = 1,
                        sep = ",", dec = ".") %>% as.matrix()
usethis::use_data(mississippi, overwrite = FALSE)
# The Zuni dataset from Peeples and Schachner 2012
zuni <- read.csv("data-raw/zuni.csv", header = TRUE, row.names = 1,
                 sep = ",", dec = ".") %>% as.matrix()
usethis::use_data(zuni, overwrite = FALSE)
