\name{EMLasso.1l.lognet.cv.param}
\alias{EMLasso.1l.lognet.cv.param}
\alias{extract.EMLasso.1l.lognet.cv.param}
\alias{length.EMLasso.1l.lognet.cv.param}
\title{Collect parameters to EMLasso.1l.lognet.cv into one list}
\usage{
  EMLasso.1l.lognet.cv.param(dfr, resp, lambda,
  nrOfSamplesPerMDRow, howManyFold = 10, groupNrPerRow =
  NULL, maxIt = 20, minIt = 10, weightsName = "weights",
  orgriName = "orgri", precalcGuidData = TRUE,
  nrOfSamplesForCriteria = nrOfSamplesPerMDRow,
  calculateCriteria = calculateCriteria.EMLasso.1l.lognet,
  verbosity = 0, reuseDebugLevel = -1, type.measure =
  "auc", grouped = TRUE, maxFracZeroesAllowed = 0,
  dfrconvprobs = dfrConversionProbs(dfr, betweenColAndLevel
  = ""))

  \method{length}{EMLasso.1l.lognet.cv.param} (x)
}
\arguments{
  \item{dfr}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{resp}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{lambda}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{nrOfSamplesPerMDRow}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{howManyFold}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{groupNrPerRow}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{maxIt}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{minIt}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{weightsName}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{orgriName}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{precalcGuidData}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{nrOfSamplesForCriteria}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{calculateCriteria}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{verbosity}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{reuseDebugLevel}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{type.measure}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{grouped}{see \code{\link{EMLasso.1l.lognet.cv}}}

  \item{maxFracZeroesAllowed}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{dfrconvprobs}{see
  \code{\link{EMLasso.1l.lognet.cv}}}

  \item{x}{\code{EMLasso.1l.lognet.cv.param} object}

  \item{i}{integer indexer}

  \item{x}{\code{EMLasso.1l.lognet.cv.param} object}
}
\value{
  object of class "EMLasso.1l.lognet.cv.param" having all
  these parameters as named members
}
\description{
  Collect parameters to \code{\link{EMLasso.1l.lognet.cv
  into one list
}
\note{
  created for use in \code{\link{run.parallel}}
}
\examples{
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
emlcvparm[1]
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
length(emlcvparm)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{run.parallel}},
  \code{\link{EMLasso.1l.lognet.cv}}

  \code{\link{cv.1l.emlasso}},
  \code{\link{EMLasso.1l.lognet}}

  \code{\link{cv.1l.emlasso}},
  \code{\link{EMLasso.1l.lognet}}
}
\keyword{constructor}
\keyword{parameter}

