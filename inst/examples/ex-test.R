## Shannon diversity test
data("merzbach", package = "folio")
merzbach_count <- as_count(merzbach)
div <- test_diversity(merzbach_count)
