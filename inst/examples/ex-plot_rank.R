## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Plot rank vs abundance
plot_rank(cantabria)

## Change graphical parameters
col <- khroma::color("bright")(5)
plot_rank(cantabria, col = col, pch = 15:19, lty = 2)
