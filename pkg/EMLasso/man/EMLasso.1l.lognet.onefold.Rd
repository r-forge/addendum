\name{EMLasso.1l.lognet.onefold}
\alias{EMLasso.1l.lognet.onefold}
\title{One fold of crossvalidate EMLasso for 1 lambda}
\usage{
  EMLasso.1l.lognet.onefold(curGroup, groupNrPerRow, dfr,
    resp, lambda, nrOfSamplesPerMDRow, maxIt, minIt,
    weightsName, orgriName, precalcGuidData, dfrconvprobs,
    reuseDebugLevel, nrOfSamplesForCriteria,
    calculateCriteria, verbosity = 0)
}
\arguments{
  \item{curGroup}{number of the fold to do}

  \item{groupNrPerRow}{vector of groupnumbers per row of
  \code{dfr}}

  \item{dfr}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}}) to fit it to}

  \item{resp}{outcome vector}

  \item{lambda}{the single lambda}

  \item{nrOfSamplesPerMDRow}{For rows with missing data,
  how many rows to sample}

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

  \item{dfrconvprobs}{dataset conversion properties (see
  \code{\link{dfrConversionProbs}})}

  \item{reuseDebugLevel}{(for internal use only)}

  \item{nrOfSamplesForCriteria}{similar to
  nrOfSamplesPerMDRow, but for final calculation of
  validation criteria}

  \item{calculateCriteria}{function to calculate criteria}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  An object of class "cvpart.emlasso", see the
  \code{actualfits} item of \code{\link{cv.1l.emlasso}}
}
\description{
  One fold of crossvalidate EMLasso for 1 lambda
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{nrs<-nrow(aDfr.MD)
uniqueGroupNrs<-seq(10)
groupNrPerRow<-similarSizeGroups(10, nobs=nrs, rand = TRUE)
eml1ffit<-EMLasso.1l.lognet.onefold(1, groupNrPerRow, aDfr.MD, y, lambda=0.05,
	nrOfSamplesPerMDRow=7, maxIt=20, minIt=10, weightsName="weights", orgriName="orgri",
	precalcGuidData=FALSE, reuseDebugLevel=-1,
	nrOfSamplesForCriteria=7, calculateCriteria=calculateCriteria.EMLasso.1l.lognet,verbosity=10)}
eml1ffit<-eml17$actualfits[[1]]
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{crossvalidate}
\keyword{EMLasso}

