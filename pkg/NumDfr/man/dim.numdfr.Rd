\name{dim.numdfr}
\alias{dim.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dimensions of an Object
}
\description{
Retrieve the dimension of a \code{\link{numdfr}} object.
}
\usage{
\method{dim}{numdfr}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object
}
}
\value{
vector of length 2, holding the number of rows and number of columns (in that order)
}
\author{
Nick Sabbe
}
\note{
Replacement method is not present, so watch out!!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{dim}}
}
\examples{
iris.nd<-numdfr(iris)
dim(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dimension }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
