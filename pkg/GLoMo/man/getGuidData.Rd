\name{getGuidData}
\alias{getGuidData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Unique identifiers for all rows of dfr and their matches in glomo
}
\description{
Unique identifiers for all rows of dfr and their matches in glomo
}
\usage{
getGuidData(glomo, dfr, guidPerObservation = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glomo}{
GLoMo object (result of \code\link{GLoMo}} call with a 'matching' dfr)
}
  \item{dfr}{
\code\link{data.frame}} or \code\link{numdfr}} to get the uids for.
}
  \item{guidPerObservation}{
if some of the information is already present (e.g. the uids of the rows, you can pass them here)
}
}
\details{
These uids are built with the function \code{\link{categoricalUniqueIdentifiers}}
}
\value{
List object of class \code{GuidData} with items:
  \item{guidPerObservation }{Unique identifier (char vector) for each row in dfr}
  \item{possibleGlomoGuidPerObs }{list with for each row of dfr the rows in glomo that can be used for it}
  \item{separator }{The separator that was used (in glomo and now in the new uids)}
}
\author{
Nick Sabbe
}

\seealso{
\code{\link{GLoMo-package}}, \code{\link{NumDfr}}, \code{\link{categoricalUniqueIdentifiers}}
}
\examples{
iris.md<-randomNA(iris, 0.1)
iris.md.nd<-numdfr(iris.md)
iris.nd<-numdfr(iris)
iris.nd.rnd<-rCatsAndCntInDfr(iris.nd, orgriName=NULL, verbosity=1)
iris.weights<-unlist(iris.nd.rnd[,"weights", drop=TRUE])
iris.nd.rnd<-iris.nd.rnd[,1:5]
iris.glomo<-GLoMo(iris.nd.rnd, weights=iris.weights, verbosity=1)
iris.guids<-getGuidData(iris.glomo, dfr=iris.md.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ uid }
\keyword{ GLoMo }% __ONLY ONE__ keyword per line
