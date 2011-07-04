\name{outlierIndices}
\alias{outlierIndices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds outliers (outside the whiskers of a boxplot)
}
\description{
Indices of values outside the whiskers of a boxplot (i.e. 1.5 times the IQR from the median) and some other similar statistics.
}
\usage{
outlierIndices(cvar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvar}{
vector of variables to investigate
}
}
\value{
Returns a list:
  \item{indices }{Indices of the outliers}
  \item{Q1 }{First quartile}
  \item{Q3 }{Third Quartile}
  \item{IQR }{Inter-quartile range}
  \item{whiskl }{Left whisker value}
  \item{whiskr }{Right whisker value}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
somedata<-c(-10, 1:10,20)
outlierIndices(somedata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~outlier }
\keyword{ ~boxplot }
\keyword{ ~whisker }
\keyword{ ~IQR }

