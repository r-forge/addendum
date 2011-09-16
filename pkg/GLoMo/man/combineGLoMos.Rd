\name{combineGLoMos}
\alias{combineGLoMos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine a set of similar GLoMo objects into 1
}
\description{
In a crossvalidation setting or similar, often similar GLoMo objects occur:
matching the same form of dataset, each of equal size or at least very similar.
This is a method that combines them reasonably into 1 GLoMo model object.
}
\usage{
combineGLoMos(..., verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
several \code{\link{GLoMo}} objects from similar datasets
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\value{
a \code{\link{GLoMo}} object
}
\references{
"Statistical Analysis with Missing Values"
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{GLoMo}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd.rnd<-rCatsAndCntInDfr(iris.md.nd, orgriName=NULL, verbosity=1)
iris.weights<-iris.nd.rnd$weights
iris.nd.rnd<-iris.nd.rnd[,1:5]
halfrows<-1:(as.integer(nrow(iris.nd.rnd)/2))
iris.glomo1<-GLoMo(iris.nd.rnd[halfrows,], weights=iris.weights[halfrows], verbosity=1)
iris.glomo2<-GLoMo(iris.nd.rnd[-halfrows,], weights=iris.weights[-halfrows], verbosity=1)

iris.glomo<-combineGLoMos(iris.glomo1, iris.glomo2, verbosity=1)
iris.pred.cond<-predict(iris.glomo, nobs=100, verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ combine }
\keyword{ General }
\keyword{ Location }
\keyword{ Model }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line
