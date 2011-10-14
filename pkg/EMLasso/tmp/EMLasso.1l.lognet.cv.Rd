\name{EMLasso.1l.lognet.cv}
\alias{EMLasso.1l.lognet.cv}
\title{Crossvalidate EMLasso for 1 lambda}
\usage{
  EMLasso.1l.lognet.cv(dfr, resp, lambda,
  nrOfSamplesPerMDRow, howManyFold = 10, groupNrPerRow =
  NULL, maxIt = 20, minIt = 10, weightsName = "weights",
  orgriName = "orgri", precalcGuidData = TRUE,
  nrOfSamplesForCriteria = nrOfSamplesPerMDRow,
  calculateCriteria = calculateCriteria.EMLasso.1l.lognet,
  verbosity = 0, reuseDebugLevel = -1, type.measure =
  "auc", grouped = TRUE, maxFracZeroesAllowed = 0,
  dfrconvprobs)
}
\arguments{
  \item{dfr}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}}) to fit it to}

  \item{resp}{outcome vector}

  \item{lambda}{the single lambda}

  \item{nrOfSamplesPerMDRow}{For rows with missing data,
  how many rows to sample}

  \item{howManyFold}{how many-fold crossvalidation}

  \item{groupNrPerRow}{as alternative to howManyFold:
  provide group number per row}

  \item{minIt}{minimum number of iterations before
  convergence is possible}

  \item{maxIt}{maximum number of iterations before
  convergence is automatically assumed}

  \item{weightsName}{name that can be safely given to a
  weights column (should not conflict with existing column
  names)}

  \item{orgriName}{name that can be safely given to a
  original row index column (should not conflict with
  existing column names)}

  \item{precalcGuidData}{(experimental) if \code{TRUE} an
  effort is made to reuse GuidData}

  \item{nrOfSamplesForCriteria}{similar to
  nrOfSamplesPerMDRow, but for final calculation of
  validation criteria}

  \item{calculateCriteria}{function to calculate criteria}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{reuseDebugLevel}{(for internal use only)}

  \item{type.measure}{one of the measures
  \code{\link{cv.glmnet}}}

  \item{grouped}{see \code{\link{cv.glmnet}}}

  \item{maxFracZeroesAllowed}{used for final set of
  coefficients -> only return the ones that were zero in at
  most this percentage of the folds}

  \item{dfrconvprobs}{dataset conversion properties (see
  \code{\link{dfrConversionProbs}})}
}
\value{
  An object of class \code{\link{cv.1l.emlasso}}
}
\description{
  Crossvalidate EMLasso for 1 lambda
}
\examples{
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvfit<-EMLasso.1l.lognet.cv(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  [PENDING]
}
\keyword{crossvalidate}
\keyword{EMLasso}

