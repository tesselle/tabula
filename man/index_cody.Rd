% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/diversity_beta.R
\docType{methods}
\name{index_cody}
\alias{index_cody}
\alias{index_cody-method}
\alias{index_cody,matrix-method}
\title{Cody Measure}
\usage{
index_cody(x, ...)

\S4method{index_cody}{matrix}(x)
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
Cody Measure
}
\details{
This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
follows the progression along the gradient/transect.

Data are standardized on a presence/absence scale (\eqn{0}/\eqn{1})
beforehand.
}
\references{
Cody, M. L. (1975). Towards a theory of continental species diversity: Bird
distributions over Mediterranean habitat gradients. \emph{In} M. L. Cody &
J. M. Diamond (Eds.), \emph{Ecology and Evolution of Communities}.
Cambridge, MA: Harvard University Press, p. 214-257.
}
\seealso{
Other beta diversity measures: 
\code{\link{index_binomial}()},
\code{\link{index_brainerd}()},
\code{\link{index_bray}()},
\code{\link{index_jaccard}()},
\code{\link{index_morisita}()},
\code{\link{index_routledge}},
\code{\link{index_sorensen}()},
\code{\link{index_whittaker}()},
\code{\link{index_wilson}()}
}
\author{
N. Frerebeau
}
\concept{beta diversity measures}
