\name{allLevels}
\alias{allLevels}
\alias{allLevels.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
return a list holding all levels for all columns
}
\description{
return a list holding all levels for all columns
}
\usage{
\method{allLevels}{data.frame}(x, onlyNonEmpty=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The \code{\link{data.frame}} to find all levels of
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
allLevels(iris)
allLevels(iris, onlyNonEmpty=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ levels }
\keyword{ list }
