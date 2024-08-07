% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plot_diceleraas.R
\docType{methods}
\name{plot_diceleraas}
\alias{plot_diceleraas}
\alias{plot_diceleraas-method}
\alias{plot_diceleraas,matrix-method}
\alias{plot_diceleraas,data.frame-method}
\title{Dice-Leraas Diagram}
\usage{
plot_diceleraas(object, ...)

\S4method{plot_diceleraas}{matrix}(
  object,
  main = NULL,
  sub = NULL,
  ann = graphics::par("ann"),
  axes = TRUE,
  frame.plot = FALSE,
  panel.first = NULL,
  panel.last = NULL,
  ...
)

\S4method{plot_diceleraas}{data.frame}(
  object,
  main = NULL,
  sub = NULL,
  ann = graphics::par("ann"),
  axes = TRUE,
  frame.plot = FALSE,
  panel.first = NULL,
  panel.last = NULL,
  ...
)
}
\arguments{
\item{object}{A \eqn{m \times p}{m x p} \code{numeric} \code{\link{matrix}} or
\code{\link{data.frame}} of count data (absolute frequencies giving the number of
individuals for each category, i.e. a contingency table). A \code{\link{data.frame}}
will be coerced to a \code{numeric} \code{matrix} via \code{\link[=data.matrix]{data.matrix()}}.}

\item{...}{Further \link[graphics:par]{graphical parameters}.}

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
}
\value{
\code{plot_diceleraas()} is called for its side-effects: it results in a
graphic being displayed (invisibly returns \code{object}).
}
\description{
Plots a Dice-Leraas diagram.
}
\details{
In a Dice-Leraas diagram, the horizontal line represents the range of data
(min-max) and the small vertical line indicates the mean. The black
rectangle is twice the standard error on the mean, while the white rectangle
is one standard deviation on either side of the mean.
}
\examples{
## Data from Desachy 2004
data("compiegne", package = "folio")

## Plot a Dice-Leraas diagram
plot_diceleraas(compiegne)
}
\references{
Dice, L. R., & Leraas, H. J. (1936). A Graphic Method for Comparing Several
Sets of Measurements. \emph{Contributions from the Laboratory of Vertebrate
Genetics}, 3: 1-3.

Hubbs, C. L., & C. Hubbs (1953). An Improved Graphical Analysis and
Comparison of Series of Samples. \emph{Systematic Biology}, 2(2): 49-56.
\doi{10.2307/sysbio/2.2.49}.

Simpson, G. G., Roe, A., & Lewontin, R. C. \emph{Quantitative Zoology}.
New York: Harcourt, Brace and Company, 1960.
}
\seealso{
Other plot methods: 
\code{\link{matrigraph}()},
\code{\link{plot_bertin}()},
\code{\link{plot_ford}()},
\code{\link{plot_heatmap}()},
\code{\link{plot_rank}()},
\code{\link{plot_spot}()},
\code{\link{seriograph}()}
}
\author{
N. Frerebeau
}
\concept{plot methods}
