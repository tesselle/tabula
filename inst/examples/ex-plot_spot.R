## Plot spot diagram...

## ...of count data...
mississippi_count <- codex::as_count(mississippi)
### ...without threshod
plot_spot(mississippi_count)
### ...with the  column means as threshold
plot_spot(mississippi_count, threshold = mean)
### ...with the column medians as threshold
plot_spot(mississippi_count, threshold = median)

## ...of a similarity matrix
sim <- similarity(mississippi_count, method = "brainerd")
plot_spot(sim)

## ...of a co-occurrence matrix
occ <- codex::as_occurrence(mississippi)
plot_spot(occ)
