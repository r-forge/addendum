\name{EMLasso.1l.lognet}
\alias{EMLasso.1l.lognet}
\title{Fit EMLasso for 1 lambda}
\usage{
  EMLasso.1l.lognet(dfr, resp, lambda, nrOfSamplesPerMDRow,
  rowsToUseForFit = seq(nrow(dfr)), maxIt = 20, minIt = 10,
  weightsName = "weights", orgriName = "orgri",
  precalcGuidData = TRUE, dfrconvprobs, verbosity = 0,
  reuseDebugLevel = -1)
}
\arguments{
  \item{dfr}{dataset (\code{\link{numdfr}} or
  \code{\link{data.frame}}) to fit it to}

  \item{resp}{outcome vector}

  \item{lambda}{the single lambda}

  \item{nrOfSamplesPerMDRow}{For rows with missing data,
  how many rows to sample}

  \item{rowsToUseForFit}{Which of the rows of dfr/resp can
  be used for fitting the LASSO}

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

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{reuseDebugLevel}{(for internal use only)}
}
\value{
  An object of class cv.1l.lognet. See
  \code{\link{cv.1l.emlasso}}
}
\description{
  Fit EMLasso for 1 lambda
}
\note{
  Contrary to full.glmnet.EM.fit, this function assumes the
  factors are already correctly assigned rowsToUseForFit
  should hold the row indices in dfr that can be used for
  fitting the glmnet
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
emlfit<-eml17$actualfits[[1]]$fitinfo
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  [PENDING]
}
\keyword{EMLasso}
\keyword{fit}

