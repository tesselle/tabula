## Create a count data matrix
A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                  nrow = 10, ncol = 10, byrow = TRUE)

## Set geographic coordinates
set_epsg(A1) <- 4326
set_coordinates(A1) <- list(X = sample(0:10, 10, TRUE),
                            Y = sample(0:10, 10, TRUE))
get_coordinates(A1)

\donttest{
## Convert to a sf object (the sf package must be installed on your machine)
# feat <- get_features(A1)
# sf::st_as_sf(feat, crs = 4326, coords = c("X", "Y"), dim = "XY",
#              remove = FALSE, na.fail = TRUE)
}
