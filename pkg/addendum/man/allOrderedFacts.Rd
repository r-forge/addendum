\name{allOrderedFacts}
\alias{allOrderedFacts}
\alias{allOrderedFacts.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
return a vector showing which columns are ordered factors
}
\description{
return a vector showing which columns are ordered factors
}
\usage{
\method{allOrderedFacts}{data.frame}(x, onlyNonEmpty=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The \code{\link{data.frame}} to investigate
}
  \item{onlyNonEmpty}{
if \code{TRUE} only the levels for categorical (factor-like) columns are in the
	resulting list. Otherwise, these are simply shown as \code{FALSE}. Default is \code{FALSE}.
}
}
\value{
logical vector.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
allOrderedFacts(iris)
allOrderedFacts(iris, onlyNonEmpty=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ levels }
\keyword{ ordered }
\keyword{ factor }
