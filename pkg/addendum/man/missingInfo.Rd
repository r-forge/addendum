\name{missingInfo}
\alias{missingInfo}
\alias{print.missingInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find/show information on where the missing data is in a dataset
}
\description{
Number of missing values per row/column
}
\usage{
missingInfo(dfr)
\method{print}{missingInfo}(x, minMissPerRow=1, minMissPerCol=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
The \code{\link{data.frame}} to find missing values (\code{NA}) in
}
  \item{x}{
The \code{missingInfo} object to display
}
  \item{minMissPerRow}{
Only show/return rows with at least this many missing values
}
  \item{minMissPerCol}{
Only show/return cols with at least this many missing values
}
  \item{\dots}{
Ignored for now
}
}
\value{
list of class \code{missingInfo}, holding:
\item{missPerCol}{named vector of number of missing values per column}
\item{missPerRow}{named vector of number of missing values per row}
\item{dim}{dimensions of the original dataset}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
iris.md<-randomNA(iris, n=0.1)
iris.mi<-missingInfo(iris.md)
print(iris.mi, minMissPerRow=2, minMissPerCol=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ missing }
