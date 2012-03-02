\name{length.numdfr}
\alias{length.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Length of an Object
}
\description{
Get or set the length of \code{\link{numdfr}} objects.
}
\usage{
\method{length}{numdfr}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to count the number of items of
}
}
\details{
In this case, for \code{\link{numdfr}} objects, returns the number of columns (as is the case for \code{\link{data.frame}})
}
\value{
a single number
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{length}}
}
\examples{
iris.nd<-numdfr(iris)
length(iris.nd)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ length }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
