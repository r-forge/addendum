\name{plotAndSmooth}
\alias{plotAndSmooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scatterplot and smoothed version (loess)
}
\description{
Does a normal scatterplot, and adds loess curve.
}
\usage{
plotAndSmooth(x, y, plotfunc = plot, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x-variable
}
  \item{y}{
y-variable
}
  \item{plotfunc}{
function (default=\code{plot} used to draw the scatterplot
}
  \item{\dots}{
Extra parameters passed to \code{plotfunc}
}
}
\value{
Nothing is returned
}
\author{
Nick Sabbe
}
\note{
I don't remember why I provided the parameter plotfunc (-:
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x<-runif(100)
y<-2*x+rnorm(100)
plotAndSmooth(x, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~scatterplot }
\keyword{ ~smooth }
