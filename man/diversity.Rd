% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/diversity.R
\docType{methods}
\name{diversity}
\alias{diversity}
\alias{diversity-method}
\alias{diversity,matrix-method}
\alias{diversity,data.frame-method}
\title{Alpha Diversity}
\usage{
diversity(object, ...)

\S4method{diversity}{matrix}(object, ..., evenness = FALSE, unbiased = FALSE)

\S4method{diversity}{data.frame}(object, ..., evenness = FALSE, unbiased = FALSE)
}
\arguments{
\item{object}{A \eqn{m \times p}{m x p} \code{numeric} \code{\link{matrix}} or
\code{\link{data.frame}} of count data (absolute frequencies giving the number of
individuals for each category, i.e. a contingency table). A \code{\link{data.frame}}
will be coerced to a \code{numeric} \code{matrix} via \code{\link[=data.matrix]{data.matrix()}}.}

\item{...}{Currently not used.}

\item{evenness}{A \code{\link{logical}} scalar: should an evenness measure be computed
instead of an heterogeneity/dominance index? Only available for \code{shannon},
\code{simpson} and \code{brillouin} indices.}

\item{unbiased}{A \code{\link{logical}} scalar: should the bias-corrected estimator be
used? Only available for \code{shannon}, \code{simpson} and \code{chao1} indices.}
}
\value{
A \code{\link{data.frame}} with the following columns:
\describe{
\item{\code{size}}{Sample size.}
\item{\code{observed}}{Number of observed taxa/types.}
\item{\code{shannon}}{\link[=index_shannon]{Shannon-Wiener diversity index}.}
\item{\code{brillouin}}{\link[=index_brillouin]{Brillouin diversity index}.}
\item{\code{simpson}}{\link[=index_simpson]{Simpson dominance index}.}
\item{\code{berger}}{\link[=index_berger]{Berger-Parker dominance index}.}
\item{\code{menhinick}}{\link[=index_menhinick]{Menhinick richness index}.}
\item{\code{margalef}}{\link[=index_margalef]{Margalef richness index}.}
\item{\code{chao1}}{\link[=index_chao1]{Chao1 estimator}.}
\item{\code{ace}}{\link[=index_ace]{Abundance-based Coverage Estimator}.}
\item{\code{squares}}{\link[=index_squares]{Squares estimator}.}
}
}
\description{
Computes multiple alpha diversity indices.
}
\details{
Alpha diversity refers to diversity at the local level, assessed within a
delimited system. It is the diversity within a uniform habitat of fixed size.

\emph{Diversity} measurement assumes that all individuals in a specific
taxa are equivalent and that all types are equally different from each
other (Peet 1974). A measure of diversity can be achieved by using indices
built on the relative abundance of taxa. These indices (sometimes referred
to as non-parametric indices) benefit from not making assumptions about the
underlying distribution of taxa abundance: they only take relative
abundances of the species that are present and species richness into
account. Peet (1974) refers to them as indices of
\emph{\link[=heterogeneity]{heterogeneity}}.

Diversity indices focus on one aspect of the taxa abundance and emphasize
either \emph{\link[=richness]{richness}} (weighting towards uncommon taxa) or
\emph{dominance} (weighting towards abundant taxa; Magurran 1988).

\emph{\link[=evenness]{Evenness}} is a measure of how evenly individuals are
distributed across the sample.
}
\note{
The \code{berger} and \code{simpson} methods return a \emph{dominance} index, not the
reciprocal or inverse form usually adopted, so that an increase in the value
of the index accompanies a decrease in diversity.
}
\examples{
## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Alpha diversity
diversity(cantabria)

## Shannon diversity index
(h <- heterogeneity(cantabria, method = "shannon"))
(e <- evenness(cantabria, method = "shannon"))

as.data.frame(h)
}
\references{
Magurran, A. E. (1988). \emph{Ecological Diversity and its Measurement}.
Princeton, NJ: Princeton University Press.
\doi{10.1007/978-94-015-7358-0}.

Peet, R. K. (1974). The Measurement of Species Diversity. \emph{Annual Review of
Ecology and Systematics}, 5(1), 285-307.
\doi{10.1146/annurev.es.05.110174.001441}.
}
\seealso{
Other diversity measures: 
\code{\link{evenness}()},
\code{\link{heterogeneity}()},
\code{\link{occurrence}()},
\code{\link{plot.DiversityIndex}()},
\code{\link{plot.RarefactionIndex}()},
\code{\link{profiles}()},
\code{\link{rarefaction}()},
\code{\link{richness}()},
\code{\link{she}()},
\code{\link{similarity}()},
\code{\link{simulate}()},
\code{\link{turnover}()}
}
\author{
N. Frerebeau
}
\concept{diversity measures}
