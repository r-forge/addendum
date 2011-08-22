\name{showNumericalHeatMap}
\alias{showNumericalHeatMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Heat map for a matrix (R)
}
\description{
Heat map for a matrix (R)
}
\usage{
showNumericalHeatMap(R, ncolors = 2, showLegend = TRUE, roundLegend = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{
Matrix for which the heatmap is shown
}
  \item{ncolors}{
in how many pieces is the range of R values subdivided
}
  \item{showLegend}{
if TRUE, adds a legend mapping colors to R values
}
  \item{roundLegend}{
if the legend is shown, round R values up to so many digits
}
}
\value{
return value of actual \code{\link{image}} call
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{image}}
}
\examples{
x<-matrix(runif(100), nrow=20)
anR<-x \%*\% t(x)
showNumericalHeatMap(anR, ncolors = 5, showLegend = TRUE, roundLegend = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~heatmap }
