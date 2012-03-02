\name{validationOutAndPredProb}
\alias{validationOutAndPredProb}
\alias{validationOutAndPredProb.cv.1l.emlasso}
\alias{validationOutAndPredProb.cvpart.emlasso}
\title{Predict probabilities from a model with its own validation sample}
\usage{
  validationOutAndPredProb(object, ...)

  \method{validationOutAndPredProb}{cvpart.emlasso}(object,
    ...)

  \method{validationOutAndPredProb}{cv.1l.emlasso} (object,
    ...)
}
\arguments{
  \item{object}{model fit}

  \item{\dots}{for flexibility in 'derived' implementation}
}
\value{
  depends on the implementation, but typically a
  \code{data.frame} with columns trueOut= true outcome for
  each row, predProb= predicted probability of level 2 for
  each row, (predProb_LR: optional: predicted probability
  from logistic regression with the selected predictors),
  weight= weight apointed to each row
}
\description{
  Predict probabilities from a model with its own
  validation sample
}
\note{
  aids to generalize validation
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
validationOutAndPredProb(eml1ffit)
data(emlcvfit, package="EMLasso")
\dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
emlfit<-eml17
validationOutAndPredProb(emlfit)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l.lognet.onefold}}

  \code{\link{cv.1l.emlasso}},
  \code{\link{EMLasso.1l.lognet}}
}
\keyword{model}
\keyword{probability}

