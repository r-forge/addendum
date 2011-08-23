\name{dimnames.numdfr}
\alias{dimnames.numdfr}
\alias{dimnames<-.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dimnames of an Object
}
\description{
Retrieve or set the dimnames of a \code{\link{numdfr}} object.
}
\usage{
\method{dimnames}{numdfr}(x)
\method{dimnames}{numdfr}(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to get/set te dimnames of.
}
  \item{value}{
new value for the dimnames
}
}
\value{
list of two (possibly named) items, the first one holding the row names (if any),
the second one holding the row names.
}
\author{
Nick Sabbe
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{dimnames}}
}
\examples{
iris.nd<-numdfr(iris)
dimnames(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dimension }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
