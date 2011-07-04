\name{squareLikeLayoutForNGraphs}
\alias{squareLikeLayoutForNGraphs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare optimized layout for given number of graphs
}
\description{
Divide the screen in parts for \code{n} plots, with number of rows and number of columns as close as possible.
}
\usage{
squareLikeLayoutForNGraphs(n, useLayout = TRUE, traceRes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Prepare for how many plots
}
  \item{useLayout}{
if TRUE (default), \code{layout} is used, otherwise \code{par}.
}
  \item{traceRes}{
if TRUE, display how many rows/cols are created
}
}
\value{
vector with two elements the number of rows and the number of columns
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
squareLikeLayoutForNGraphs(8)
for(i in 1:8){
	plot(runif(100), runif(100), xlab="x", ylab="y")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~plot }
\keyword{ ~multiple }
\keyword{ ~layout }
