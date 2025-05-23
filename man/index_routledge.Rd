% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/diversity_beta.R
\docType{methods}
\name{index_routledge}
\alias{index_routledge}
\alias{index_routledge1}
\alias{index_routledge1-method}
\alias{index_routledge2}
\alias{index_routledge2-method}
\alias{index_routledge3}
\alias{index_routledge3-method}
\alias{index_routledge1,matrix-method}
\alias{index_routledge2,matrix-method}
\alias{index_routledge3,matrix-method}
\title{Routledge Measures}
\usage{
index_routledge1(x, ...)

index_routledge2(x, ...)

index_routledge3(x, ...)

\S4method{index_routledge1}{matrix}(x)

\S4method{index_routledge2}{matrix}(x)

\S4method{index_routledge3}{matrix}(x)
}
\arguments{
\item{x}{A \eqn{m \times p}{m x p} \code{numeric} \code{\link{matrix}} of count data
(absolute frequencies, i.e. a contingency table).}

\item{...}{Currently not used.}
}
\value{
A \code{\link{numeric}} vector.
}
\description{
Routledge Measures
}
\details{
This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
follows the progression along the gradient/transect.

Data are standardized on a presence/absence scale (\eqn{0}/\eqn{1})
beforehand.
}
\references{
Routledge, R. D. (1977). On Whittaker's Components of Diversity.
\emph{Ecology}, 58(5), 1120-1127. \doi{10.2307/1936932}.
}
\seealso{
Other beta diversity measures: 
\code{\link{index_binomial}()},
\code{\link{index_brainerd}()},
\code{\link{index_bray}()},
\code{\link{index_cody}()},
\code{\link{index_jaccard}()},
\code{\link{index_morisita}()},
\code{\link{index_sorensen}()},
\code{\link{index_whittaker}()},
\code{\link{index_wilson}()}
}
\author{
N. Frerebeau
}
\concept{beta diversity measures}
