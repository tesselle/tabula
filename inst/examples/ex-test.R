## Data from Magurran 1988, p. 145-149
oakwood <- c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3,
             3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 0, 0)
spruce <- c(30, 30, 3, 65, 20, 11, 0, 4, 2, 14,
            0, 3, 9, 0, 0, 5, 0, 0, 0, 0, 1, 1)

test_shannon(oakwood, spruce)
test_simpson(oakwood, spruce)

## Data from Conkey 1980, Kintigh 1989
data("cantabria")

test_shannon(cantabria)
test_simpson(cantabria)
