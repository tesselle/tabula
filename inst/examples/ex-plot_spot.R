## Data from Huntley 2004, 2008
data("pueblo")

## Plot spot diagram of count data
plot_spot(pueblo, type = "ring")
plot_spot(pueblo, type = "plain")

## Plot conditional proportions
plot_spot(pueblo, freq = TRUE, margin = 1)
plot_spot(pueblo, freq = TRUE, margin = 2)
