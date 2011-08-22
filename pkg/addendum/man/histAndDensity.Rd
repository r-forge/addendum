\name{histAndDensity}
\alias{histAndDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot histogram with added density estimate
}
\description{
Plot a simple histogram with an estimated density line in red
}
\usage{
histAndDensity(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data vector for which the histogram is to be drawn
}
  \item{\dots}{
Extra parameters passed on to \code{hist}. Note that parameter \code{freq} is not supported, as drawing the density line requires \code{freq=FALSE}
}
}
\details{
For the density estimation, \code{density} is used with \code{na.rm=TRUE}
}
\value{
The result of the \code{hist} call
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hist}}
}
\examples{
somedata<-rnorm(100, mean=5, sd=3)
histAndDensity(somedata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~hist }
\keyword{ ~density }
\keyword{ ~histogram }
