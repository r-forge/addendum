\name{[.numdfr}
\alias{[.numdfr}
\alias{[<-.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract or Replace Parts of an Object
}
\description{
Subset \code{\link{numdfr}} object.
}
\usage{
\method{[}{numdfr}(x, i, j, returnAsMatrix = drop, drop = FALSE)
\method{[}{numdfr}(x, i, j) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object
}
  \item{i, j}{
elements to extract or replace. Numeric or character or, empty. Numeric values are coerced to integer as if by as.integer. For replacement, a logical matrix is allowed.
}
  \item{returnAsMatrix}{
if \code{TRUE}, the result is not a \code{\link{numdfr}} object, but a regular matrix. Default is \code{FALSE}, unless \code{drop} is \code{TRUE}.
}
  \item{drop}{
if \code{TRUE}, the dimsensions of size 1 are removed from the return value.
}
  \item{value}{
new values(s)
}
}
\details{
Mostly designed to work similarly to \code{\link{[.data.frame}}, but needed to provide an extra parameter \code{returnAsMatrix}.
}
\value{
Depending on the logical parameters, a \code{\link{numdfr}}, a matrix (2-dimensional) or a vector.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{[}}, \code{\link{[.data.frame}}
}
\examples{
iris.nd<-numdfr(iris)
iris.nd[1,1]
iris.nd[1,1]<-8
iris.nd[1,1]
iris.nd[,"Sepal.Length"]<-iris.nd[,"Sepal.Length", drop=TRUE]*2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ subset }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
