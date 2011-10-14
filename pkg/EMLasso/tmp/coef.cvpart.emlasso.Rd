\name{coef.cvpart.emlasso}
\alias{coef.cvpart.emlasso}
\title{Get coefficients of a "cvpart.emlasso" object}
\usage{
  \method{coef}{cvpart.emlasso} (object, onlyFinal = FALSE,
  verbosity = 0, ...)
}
\arguments{
  \item{object}{"cvpart.emlasso" object}

  \item{onlyFinal}{if \code{TRUE}, only the coefficients of
  the final iteration are returned}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{\dots}{ignored}
}
\value{
  \code{\link{data.frame}} with a row per iteration and a
  column per coefficient
}
\description{
  Get coefficients of a "cvpart.emlasso" object
}
\examples{
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
nrs<-nrow(dfr)
uniqueGroupNrs<-seq(10)
groupNrPerRow<-similarSizeGroups(10, nobs=nrs, rand = TRUE)
eml1ffit<-EMLasso.1l.lognet.onefold(1, groupNrPerRow, aDfr, y, lambda=0.05,
nrOfSamplesPerMDRow=7, maxIt=20, minIt=10, weightsName="weights", orgriName="orgri",
precalcGuidData=FALSE, reuseDebugLevel=-1,
nrOfSamplesForCriteria=7, calculateCriteria=calculateCriteria.EMLasso.1l.lognet,verbosity=10)
coef(eml1ffit)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{coefficient}

