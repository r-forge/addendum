\name{names.numdfr}
\alias{names.numdfr}
\alias{names<-.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Names of an Object
}
\description{
Functions to get or set the names of a \code{\link{numdfr}} object.
}
\usage{
\method{names}{numdfr}(x)
\method{names}{numdfr}(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to get/set names of
}
  \item{value}{
new names
}
}
\value{
Similarly to the \code{\link{data.frame}} implementation, returns a character
vector of column names.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{names}}
}
\examples{
iris.nd<-numdfr(iris)
names(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ names }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
