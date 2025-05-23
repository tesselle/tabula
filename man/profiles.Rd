% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plot_diversity.R
\docType{methods}
\name{profiles}
\alias{profiles}
\alias{profiles-method}
\alias{profiles,matrix-method}
\alias{profiles,data.frame-method}
\title{Diversity Profiles}
\usage{
profiles(object, ...)

\S4method{profiles}{matrix}(
  object,
  alpha = seq(from = 0, to = 4, by = 0.04),
  color = NULL,
  symbol = FALSE,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  sub = NULL,
  ann = graphics::par("ann"),
  axes = TRUE,
  frame.plot = axes,
  panel.first = NULL,
  panel.last = NULL,
  legend = list(x = "topright"),
  ...
)

\S4method{profiles}{data.frame}(
  object,
  alpha = seq(from = 0, to = 4, by = 0.04),
  color = NULL,
  symbol = FALSE,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  sub = NULL,
  ann = graphics::par("ann"),
  axes = TRUE,
  frame.plot = axes,
  panel.first = NULL,
  panel.last = NULL,
  legend = list(x = "topright"),
  ...
)
}
\arguments{
\item{object}{A \eqn{m \times p}{m x p} \code{numeric} \code{\link{matrix}} or
\code{\link{data.frame}} of count data (absolute frequencies giving the number of
individuals for each category, i.e. a contingency table). A \code{\link{data.frame}}
will be coerced to a \code{numeric} \code{matrix} via \code{\link[=data.matrix]{data.matrix()}}.}

\item{...}{Further \link[graphics:par]{graphical parameters} to be passed to
\code{\link[graphics:lines]{graphics::lines()}}}

\item{alpha}{A \code{\link{numeric}} vector giving the values of the alpha parameter.}

\item{color}{A vector of colors (will be mapped to the rownames of \code{object}).
If \code{color} is a named a named vector, then the colors will be associated
with the rownames of \code{object}. Ignored if set to \code{FALSE}.}

\item{symbol}{A specification for the line type (will be mapped to
the rownames of \code{object}). If \code{symbol} is a named a named vector, then the
line types will be associated with the rownames of \code{object}.
Ignored if set to \code{FALSE}.}

\item{xlab, ylab}{A \code{\link{character}} vector giving the x and y axis labels.}

\item{main}{A \code{\link{character}} string giving a main title for the plot.}

\item{sub}{A \code{\link{character}} string giving a subtitle for the plot.}

\item{ann}{A \code{\link{logical}} scalar: should the default annotation (title and x,
y and z axis labels) appear on the plot?}

\item{axes}{A \code{\link{logical}} scalar: should axes be drawn on the plot?}

\item{frame.plot}{A \code{\link{logical}} scalar: should a box be drawn around the
plot?}

\item{panel.first}{An an \code{expression} to be evaluated after the plot axes are
set up but before any plotting takes place. This can be useful for drawing
background grids.}

\item{panel.last}{An \code{expression} to be evaluated after plotting has taken
place but before the axes, title and box are added.}

\item{legend}{A \code{\link{list}} of additional arguments to be passed to
\code{\link[graphics:legend]{graphics::legend()}}; names of the list are used as argument names.
If \code{NULL}, no legend is displayed.}
}
\value{
\code{profiles()} is called for its side-effects: it results in a graphic being
displayed (invisibly returns \code{object}).
}
\description{
Diversity Profiles
}
\details{
If the profiles cross, the diversities are non-comparable across samples.
}
\examples{
## Replicate fig. 1 of Tóthmérész 1995
spc <- matrix(
  data = c(33, 29, 28, 5, 5, 0, 0, 42, 30, 10,
           8, 5, 5, 0, 32, 21, 16, 12, 9, 6, 4),
  nrow = 3, byrow = TRUE, dimnames = list(c("Z", "B", "C"), NULL)
)

profiles(spc, color = color("bright"))
}
\references{
Tóthmérész, B. (1995). Comparison of Different Methods for Diversity
Ordering. \emph{Journal of Vegetation Science}, 6(2), 283-290.
\doi{10.2307/3236223}.
}
\seealso{
Other diversity measures: 
\code{\link{diversity}()},
\code{\link{evenness}()},
\code{\link{heterogeneity}()},
\code{\link{occurrence}()},
\code{\link{plot.DiversityIndex}()},
\code{\link{plot.RarefactionIndex}()},
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
