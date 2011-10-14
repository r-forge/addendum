\name{getBeta.cv.emlasso}
\alias{getBeta.cv.emlasso}
\title{getBeta implementation for class cv.emlasso}
\usage{
  \method{getBeta}{cv.emlasso} (object)
}
\arguments{
  \item{object}{\code{\link{cv.emlasso}} object}
}
\value{
  see \code{\link{getBeta}}
}
\description{
  \code{\link{getBeta}} implementation for class
  \code{\link{cv.emlasso}}
}
\note{
  needed to make \code{\link{plotex}} etc. work seemlessly
}
\examples{
aDfr<-numdfr(randomNA(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE), n=0.05))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvfit<-EMLasso.lognet.cv(aDfr, y, lambda=c(0.05, 0.03), nrOfSamplesPerMDRow=7, verbosity=10)
getBeta(emlcvfit$result)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{plotex}}
}
\keyword{plotex}

