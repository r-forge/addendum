\name{rbind.numdfr}
\alias{rbind.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
add \code{numdfr} objects' rows together
}
\description{
Combines all of its parameters to one big \code{\link{numdfr}} object by setting
all the rows below one another.
}
\usage{
\method{rbind}{numdfr}(..., ensure.unique.rownames=FALSE, separator=".",
	postfixcol=NULL, allowemptypostfix=TRUE, deparse.level = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
several \code{\link{numdfr}} objects to combine.
}
  \item{ensure.unique.rownames}{
if \code{TRUE} and rownames are present, \code{\link{postfixToMakeUnique}} is
called afterwards to ensure that these are unique.
}
  \item{separator, postfixcol, allowemptypostfix}{
see \code{\link{postfixToMakeUnique}}.
}
  \item{deparse.level}{
integer controlling the construction of labels in the case of non-matrix-like arguments. See \code{\link{rbind.data.frame}}.
}
}
\value{
\code{\link{numdfr}} object
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{rbind}}, \code{\link{rbind.data.frame}}
}
\examples{
iris.nd<-numdfr(iris)[1:2,]
iris.nd2<-rbind(iris.nd, iris.nd, iris.nd)
rownames(iris.nd2)
iris.nd2<-rbind(iris.nd, iris.nd, iris.nd, ensure.unique.rownames=TRUE)
rownames(iris.nd2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ row }
\keyword{ combine }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
