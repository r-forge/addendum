\name{is.numdfr}
\alias{is.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check whether object is \code{numdfr}
}
\description{
Finds out whether a given object is of class \code{\link{numdfr}}
}
\usage{
is.numdfr(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object to investigate
}
}

\value{
\code{TRUE} or \code{FALSE}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{is}}
}
\examples{
is.numdfr(iris)
iris.nd<-numdfr(iris)
is.numdfr(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ numdfr }
