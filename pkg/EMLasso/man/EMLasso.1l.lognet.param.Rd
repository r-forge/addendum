\name{EMLasso.1l.lognet.param}
\alias{[.EMLasso.1l.lognet.param}
\alias{EMLasso.1l.lognet.param}
\alias{length.EMLasso.1l.lognet.param}
\title{Collect parameters to EMLasso.1l.lognet into one list}
\usage{
  EMLasso.1l.lognet.param(dfr, resp, lambda,
    nrOfSamplesPerMDRow = 10,
    rowsToUseForFit = seq(nrow(dfr)), maxIt = 20,
    minIt = 10, weightsName = "weights",
    orgriName = "orgri", precalcGuidData = FALSE,
    dfrconvprobs, verbosity = 0, reuseDebugLevel = -1,
    betweenColAndLevel = "")

  \method{[}{EMLasso.1l.lognet.param}(x, i)

  \method{length}{EMLasso.1l.lognet.param} (x)
}
\arguments{
  \item{dfr}{see \code{\link{EMLasso.1l.lognet}}}

  \item{resp}{see \code{\link{EMLasso.1l.lognet}}}

  \item{lambda}{see \code{\link{EMLasso.1l.lognet}}}

  \item{nrOfSamplesPerMDRow}{see
  \code{\link{EMLasso.1l.lognet}}}

  \item{rowsToUseForFit}{see
  \code{\link{EMLasso.1l.lognet}}}

  \item{minIt}{see \code{\link{EMLasso.1l.lognet}}}

  \item{maxIt}{see \code{\link{EMLasso.1l.lognet}}}

  \item{weightsName}{see \code{\link{EMLasso.1l.lognet}}}

  \item{orgriName}{see \code{\link{EMLasso.1l.lognet}}}

  \item{precalcGuidData}{see
  \code{\link{EMLasso.1l.lognet}}}

  \item{dfrconvprobs}{see \code{\link{EMLasso.1l.lognet}}}

  \item{verbosity}{see \code{\link{EMLasso.1l.lognet}}}

  \item{reuseDebugLevel}{see
  \code{\link{EMLasso.1l.lognet}}}

  \item{betweenColAndLevel}{if \code{dfrconvprobs} was not
  provided, this separator is used (see
  \code{\link{dfrConversionProbs}})}

  \item{x}{\code{EMLasso.1l.lognet.param} object}

  \item{i}{integer indexer}
}
\value{
  object of class "EMLasso.1l.lognet.param" having all
  these parameters (except \code{betweenColAndLevel}) as
  named members
}
\description{
  Collect parameters to \code{\link{EMLasso.1l.lognet}}
  into one list
}
\note{
  created for use in \code{\link{run.parallel}}
}
\examples{
data(emlcvfit, package="EMLasso")
emlcvparm<-EMLasso.1l.lognet.cv.param(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvparm<-EMLasso.1l.lognet.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
emlcvparm[1]
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvparm<-EMLasso.1l.lognet.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
length(emlcvparm)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{run.parallel}},
  \code{\link{EMLasso.1l.lognet}}

  \code{\link{EMLasso.1l.lognet}}
}
\keyword{constructor}
\keyword{parameter}

