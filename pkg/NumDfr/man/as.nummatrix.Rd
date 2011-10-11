\name{as.nummatrix}
\alias{as.nummatrix}
\alias{as.nummatrix.numdfr}
\alias{as.nummatrix.matrix}
\alias{as.nummatrix.default}
\alias{as.nummatrix.data.frame}
\alias{as.nummatrix.numdfr.rep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert \code{object} to \code{numeric matrix}
}
\description{
Functions to coerce object to \code{numericmatrix}.
}
\usage{
\method{as.nummatrix}{numdfr}(object)
\method{as.nummatrix}{numdfr.rep}(object)
\method{as.nummatrix}{matrix}(object)
\method{as.nummatrix}{data.frame}(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object to coerce
}
}
\value{
\code{\link{matrix}} with the same dimensions as the original object,
and with factors converted to numericals. Have not always checked what
happens to characters...
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
iris.mat<-as.nummatrix(iris.nd)
iris.mat2<-as.nummatrix(iris)
#compare this to
iris.mat3<-as.matrix(iris) #becomes all character!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ numdfr }
\keyword{ matrix }
\keyword{ coerce }% __ONLY ONE__ keyword per line
