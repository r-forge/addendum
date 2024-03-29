\name{allLevels.numdfr}
\alias{allLevels.numdfr}
\alias{allOrderedFacts.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
return a list holding all levels for all columns / which cols are ordered factors
}
\description{
return a list holding all levels for all columns / which cols are ordered factors
}
\usage{
\method{allLevels}{numdfr}(x, onlyNonEmpty=FALSE)
\method{allOrderedFacts}{numdfr}(x, onlyNonEmpty=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The \code{\link{numdfr}} to find all levels of
}
  \item{onlyNonEmpty}{
if \code{TRUE} only the levels for categorical (factor-like) columns are in the
	resulting list. Otherwise, these are represented as a character vector of
	length zero. Default is \code{FALSE}.
}
}
\value{
list of character vectors.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
iris.nd<-numdfr(iris)
allLevels(iris.nd)
allLevels(iris.nd, onlyNonEmpty=TRUE)
allOrderedFacts(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ levels }
\keyword{ list }
