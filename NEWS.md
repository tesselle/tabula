# tabula 1.2.0

* ADD: Belanger and Husi (2006) dating method (#3).
* ADD: binomial co-occurrence assessment method (similarity between types).
* ADD: 'SimilarityMatrix' S4 class to represent a (dis)similarity matrix.
* ADD: 'plotSpot' method for 'SimilarityMatrix' object.
* ADD: 'plotSpot' method for 'OccurrenceMatrix' object.
* ADD: '[' methods for several classes.
* FIX: 'similarity()' now returns a SimilarityMatrix object.
* FIX: 'plotBar()' does not add confidence interval by default.
* FIX: add an argument to 'seriate()' to pass a 'BootCA' object (#4).
* FIX: add an optional progress bars with 'pbapply' in long running functions.
* FIX: deprecate useless accessors.
* FiX: 'OccurrenceMatrix' now stores the number of times each pair of taxa occurs together in at least one sample.

# tabula 1.1.0

* ADD: Chao1 estimator for abundance data.
* ADD: Bias-corrected Chao1 estimator.
* ADD: Improved Chao1 estimator.
* ADD: Chao2 estimator for replicated incidence data.
* ADD: Bias-corrected Chao2 estimator.
* ADD: Improved Chao2 estimator.
* ADD: Abundance-based Coverage Estimator (ACE).
* ADD: Incidence-based Coverage Estimator (ICE).
* FIX: 'similarity()' now returns a symmetric matrix.
* FIX: add references in the 'Description' field of the DESCRIPTION file (#1).
* FIX: split documentation for alpha-diversity measures.
* FIX: split documentation for beta-diversity measures.

# tabula 1.0.0

* ADD: Brainerd-Robinson coefficient of similarity.
* ADD: Zuni ceramics dataset.
* ADD: Mississippi ceramics dataset.
* ADD: 'BootCA' S4 class to store partial bootstrap CA results.
* ADD: extract methods for PermutationOrder and BootCA.
* ADD: vignette for matrix seriation.
* FIX: use 'stats::rmultinorm()' for partial bootstrap CA.

# tabula 0.9.0

* First release.
