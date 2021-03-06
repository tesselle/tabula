% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plot_diversity.R
\docType{methods}
\name{plot_diversity}
\alias{plot_diversity}
\alias{plot,DiversityIndex,missing-method}
\title{Diversity Plot}
\usage{
\S4method{plot}{DiversityIndex,missing}(x)
}
\arguments{
\item{x}{A \code{\linkS4class{DiversityIndex}} object to be plotted.}
}
\description{
Diversity Plot
}
\examples{
\donttest{
## Coerce data to a count matrix
data("chevelon", package = "folio")
chevelon <- as_count(chevelon)

## Assemblage diversity size comparison
## Warning: this may take a few seconds!
sim_evenness <- simulate_evenness(chevelon, method = "shannon")
plot(sim_evenness)

sim_richness <- simulate_richness(chevelon, method = "none")
plot(sim_richness)
}
}
\seealso{
\code{\link[=index_heterogeneity]{index_heterogeneity()}}, \code{\link[=index_evenness]{index_evenness()}}, \code{\link[=index_richness]{index_richness()}}

Other plot: 
\code{\link{plot_bar}},
\code{\link{plot_line}},
\code{\link{plot_matrix}},
\code{\link{plot_spot}()}
}
\author{
N. Frerebeau
}
\concept{plot}
