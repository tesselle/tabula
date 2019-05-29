## Coerce dataset to abundance (count) matrix
zuni <- as(zuni, "CountMatrix")

## Assume that some assemblages are reliably dated (this is NOT a real example)
## The name of the vector elements must match the names of the assemblages
setDates(zuni) <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

## Model the event and accumulation date for each assemblage
(model <- dateEvent(zuni, cutoff = 90))

## Plot event date and accumulation time distributions
## (for the first three assemblages)
plotDate(model, type = "activity", event = TRUE, select = 1:3) +
  ggplot2::theme_bw()

## Activity plot
plotDate(model, type = "activity") +
  ggplot2::theme_bw()
## Tempo plot
plotDate(model, type = "tempo") +
  ggplot2::theme_bw()

\donttest{
## Check model variability
## Warning: this may take a few seconds!
### Jackknife fabrics
refined_jack <- refine(model, method = "jackknife", n = 1000)
head(refined_jack)

### Bootstrap of assemblages
refined_boot <- refine(model, method = "bootstrap", n = 1000)
head(refined_boot)
}
