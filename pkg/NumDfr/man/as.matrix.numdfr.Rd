\name{as.matrix.numdfr}
\alias{as.matrix.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert \code{\link{numdfr}} (back) to \code{matrix}
}
\description{
Functions to coerce (\code{\link{numdfr}}) object to \code{matrix}.
}
\usage{
\method{as.matrix}{numdfr}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to coerce
}
  \item{\dots}{
ignored for now, but see \code{\link{as.matrix}}
}
}
\value{
\code{\link{matrix}} with the same dimensions as the original \code{\link{numdfr}},
and with factors converted to numericals.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{as.matrix}}
}
\examples{
iris.nd<-numdfr(iris)
iris.mat<-as.matrix(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ numdfr }
\keyword{ matrix }
\keyword{ coerce }% __ONLY ONE__ keyword per line
