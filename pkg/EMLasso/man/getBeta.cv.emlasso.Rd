\name{getBeta.cv.emlasso}
\alias{getBeta.cv.emlasso}
\title{getBeta implementation for class cv.emlasso}
\usage{
  \method{getBeta}{cv.emlasso} (object, type = NULL)
}
\arguments{
  \item{object}{\code{\link{cv.emlasso}} object}

  \item{type}{if equal to "final", then the beta from the
  final \code{\link{glmnet}} fit are used}
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
data(emlcvfit, package="EMLasso")
\dontrun{emlcvfit<-EMLasso.lognet.cv(aDfr.MD, y, lambda=c(0.05, 0.03), nrOfSamplesPerMDRow=7, verbosity=10)}
emlcvfitred<-reduce(emlcvfit)
getBeta(emlcvfitred)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{plotex}}
}
\keyword{plotex}

