\name{EMLasso.1l.lognet.cv}
\alias{"cv.1l.emlasso-class"}
\alias{"cvpart.emlasso-class"}
\alias{cv.1l.emlasso}
\alias{cvpart.emlasso}
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
  An object of class "cv.1l.emlasso" (where I write
  dataset, this could either be a \code{\link{data.frame}}
  or \code{\link{numdfr}} object) \enumerate{ \item
  \code{lambda} : 1 lambda value \item \code{cvm}: mean
  criterion \item \code{cvsd}: sd of criterion \item
  \code{cvup}: upper limit of criterion \item \code{cvlo}:
  lower limit of criterion \item \code{name}: name of
  criterion \item \code{actualfits}: list of fits per fold,
  each of class "cvpart.emlasso" \enumerate{ \item
  \code{fitInfo}: result of \code{\link{EMLasso.1l.lognet}}
  (of class \code{\link{EMLasso.1l.lognet}}) \item
  \code{valsample}: result of
  predict.GLoMo(returnRepeats=TRUE) (essentially imputed
  dataset of the validation part of the original data) +
  extra item resp:\enumerate{ \item \code{predicted}:
  dataset used for validation \item \code{numRepPerRow}:
  named integer vector: names are row indices in dfr,
  values are how many times this row is reused in predicted
  \item \code{resp}: outcome variable from the original
  dfr, but properly subsetted and extended so there is one
  (correct) outcome for each row of \item \code{criteria}:
  result of calculateCriteria(.EMLasso.1l.lognet): \item
  \code{predictionSummary}: rows with mcrate, fposrate,
  fnegrate and mcrate.se over a set of fixed thresholds
  (columns) from 0.05 to 0.95 by 0.05 \item \code{AUC} +
  \code{varAUC} \item \code{AUCbs} + \code{varAUCbs}: AUC +
  its variance estimated by bootstrap \item \code{N}:
  number of observations used for this validation \item
  \code{predProb}: predicted probabilities per observation
  (of valsample) of the last lasso.fit \item
  \code{predProb_logreg}: similar, but now of logreg.fit }
  } \item \code{type.measure}: passed along name of
  criterion \item \code{grouped}: passed along grouped (see
  cv.glmnet) \item \code{nzero}: mean coefficients for all
  coefficients that were at most maxFracZeroesAllowed times
  0 in the final fit for each of the folds }
}
\description{
  Crossvalidate EMLasso for 1 lambda
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlcvfit<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
emlcvfit<-eml17
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  [PENDING]
}
\keyword{crossvalidate}
\keyword{EMLasso}

