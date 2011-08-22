\name{as.list.numdfr}
\alias{as.list.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coerce to list
}
\description{
Function to coerce to R lists.
}
\usage{
\method{as.list}{numdfr}(x, returnFactors=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{\link{numdfr}} object to coerce
}
  \item{returnFactors}{
if TRUE (the default), return columns that represent factors as factors. If not, 
they are returned as numerical vectors.
}
  \item{\dots}{
ignored for now, but see \code{\link{as.list}}
}
}
\details{
Often used internally by e.g. \code{\link{lapply}}. This function is not expected
to perform swiftly, so use with caution. Mainly provided so code can be shared (at
the cost of performance) with \code{\link{data.frame}}.

See \code{\link{as.list.data.frame}} for more explanation on how this function
is supposed to work. It is not extensively tested, but appears to work in the
\code{\link{lapply}} case.
}
\value{
list consisting of the columns (either numerical or as factors), similar (equal?)
to this method for \code{\link{data.frame}}.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{as.list}}, \code{\link{as.list.data.frame}}
}
\examples{
iris.nd<-numdfr(iris)
lapply(iris, is.factor) 
lapply(iris.nd, is.factor) #should give the same result
lapply(as.list(iris.nd, returnFactors=FALSE), is.factor) #should give different result (all FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ list }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
