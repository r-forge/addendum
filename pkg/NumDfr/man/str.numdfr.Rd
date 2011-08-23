\name{str.numdfr}
\alias{str.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display contents of a \code{\link{numdf}} object
}
\description{
Display contents of a \code{\link{numdf}} object
}
\usage{
\method{str}{numdfr}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{\link{numdf}} object
}
  \item{\dots}{
ignored for now.
}
}
\value{
returns nothing (and invisibly so)
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{str}}
}
\examples{
iris.nd<-numdfr(iris)
str(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ structure }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
