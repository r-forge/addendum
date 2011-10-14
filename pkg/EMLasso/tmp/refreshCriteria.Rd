\name{refreshCriteria}
\alias{refreshCriteria}
\alias{refreshCriteria.cv.1l.emlasso}
\alias{refreshCriteria.cvpart.emlasso}
\title{S3 Generic method for recalculating criteria}
\usage{
  refreshCriteria(object, calculateCriteria =
  calculateCriteria.EMLasso.1l.lognet, verbosity = 0, ...)

  \method{refreshCriteria}{cvpart.emlasso} (object,
  calculateCriteria = calculateCriteria.EMLasso.1l.lognet,
  verbosity = 0, ...)

  \method{refreshCriteria}{cv.1l.emlasso} (object,
  calculateCriteria = calculateCriteria.EMLasso.1l.lognet,
  verbosity = 0, type.measure =
  ifelse(is.null(object$type.measure), "auc",
  object$type.measure), grouped =
  ifelse(is.null(object$type.grouped), TRUE,
  object$type.grouped), maxFracZeroesAllowed = 0, ...)
}
\arguments{
  \item{object}{object for which to recalculate the
  criteria}

  \item{calculateCriteria}{function that does the
  calculations ()defaults to
  \code{\link{calculateCriteria.EMLasso.1l.lognet}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{\dots}{passed on to deeper functions}

  \item{type.measure}{see \code{\link{cv.glmnet}}}

  \item{grouped}{see \code{\link{cv.glmnet}}}

  \item{maxFracZeroesAllowed}{see
  \code{\link{EMLasso.1l.lognet.cv}}}
}
\value{
  object similar to \code{object} but with newly calculated
  criteria

  object similar to \code{object} but with newly calculated
  criteria

  object similar to \code{object} but with newly calculated
  criteria
}
\description{
  S3 Generic method for recalculating criteria
}
\examples{
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlfit<-EMLasso.1l.lognet(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
emlfit<-refreshCriteria
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{calculateCriteria.EMLasso.1l.lognet}},
  \code{\link{EMLasso.1l.lognet.cv}}
}
\keyword{criteria}

