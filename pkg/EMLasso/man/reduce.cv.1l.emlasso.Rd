\name{reduce.cv.1l.emlasso}
\alias{reduce.cv.1l.emlasso}
\title{Reduce memory footprint of cv.1l.emlasso object}
\usage{
  \method{reduce}{cv.1l.emlasso} (object, orgdfr, ...,
  verbosity = 0)
}
\arguments{
  \item{object}{\code{\link{cv.1l.emlasso}} object to
  reduce}

  \item{orgdfr}{original dataset that was used to fit it
  to}

  \item{\dots}{provided for conformance to S3
  \code{\link{reduce}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  An object of class \code{\link{cv.1l.emlasso.reduced}}
}
\description{
  Reduce memory footprint of \code{\link{cv.1l.emlasso}}
  object
}
\note{
  mostly uses class \code{\link{numdfr.rep}}
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlcvfit<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
emlcvfit<-eml17
emlcvfit.red<-reduce(emlcvfit, verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{memory}
\keyword{reduce}

