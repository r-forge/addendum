\name{as.data.frame.numdfr}
\alias{as.data.frame.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert \code{\link{numdfr}} (back) to \code{data.frame}
}
\description{
Functions to coerce (\code{\link{numdfr}}) object to \code{data.frame} if possible
}
\usage{
\method{as.data.frame}{numdfr}(x, row.names = NULL, optional = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to coerce
}
  \item{row.names}{
ignored for now, but see \code{\link{as.data.frame}}
}
  \item{optional}{
ignored for now, but see \code{\link{as.data.frame}}
}
  \item{\dots}{
ignored for now, but see \code{\link{as.data.frame}}
}
}
\details{
In most cases, this is the inverse of \code{\link{numdfr}}.
}
\value{
\code{\link{as.data.frame}} with the same dimensions as the original \code{\link{numdfr}},
and with factors in the 'standard way'.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{as.data.frame}}
}
\examples{
iris.nd<-numdfr(iris)
iris.cpy<-as.data.frame(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ numdfr }
\keyword{ data.frame }
\keyword{ coerce }% __ONLY ONE__ keyword per line
