\name{calculateCriteria.EMLasso.1l.lognet}
\alias{calculateCriteria.EMLasso.1l.lognet}
\title{Function to calculate AUC / missclassification based on a logistic EMLasso}
\usage{
  calculateCriteria.EMLasso.1l.lognet(fitinfo, valsample,
  verbosity)
}
\arguments{
  \item{fitinfo}{\code{\link{EMLasso.1l.lognet}} object}

  \item{valsample}{dataset to use for calculating the
  criteria}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  see the \code{criteria} part of
  \code{\link{cv.1l.emlasso}}
}
\description{
  Function to calculate AUC / missclassification based on a
  logistic EMLasso
}
\note{
  Passed as default \code{calculateCriteria} parameter to a
  lot of the functions in this package
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
emlfit<-eml17$actualfits[[1]]$fitinfo
valSample<-predict(object=emlfit$glomo, nobs=5,
newdata=numdfr(aDfr.MD), returnRepeats=TRUE, verbosity=10)
valSample$resp<-rep(y, valSample$numRepPerRow)
crits<-calculateCriteria.EMLasso.1l.lognet(emlfit, valSample, verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{cv.1l.emlasso}},
  \code{\link{EMLasso.1l.lognet.cv}},\code{\link{refreshCriteria}},
  \code{\link{refreshCriteria.cvpart.emlasso}},\code{\link{refreshCriteria.cv.1l.emlasso}},
  \code{\link{EMLasso.1l.lognet.cv.param}},
}
\keyword{AUC}
\keyword{missclassification}

