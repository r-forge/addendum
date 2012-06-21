\name{as.double.numdfr}
\alias{as.double.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Double-Precision Vectors (from numdfr)
}
\description{
Coerce to double-precision vector from \code{\link{numdfr}}.
}
\usage{
\method{as.double}{numdfr}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to coerce
}
  \item{\dots}{
ignored for now
}
}
\value{
vector of type "double"
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{as.double}}
}
\examples{
iris.nd<-numdfr(iris)
as.double(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ double }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
