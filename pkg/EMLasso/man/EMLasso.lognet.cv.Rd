\name{EMLasso.lognet.cv}
\alias{EMLasso.lognet.cv}
\title{Run EMLasso.1l.lognet.cv for several lambda values through snowFall}
\usage{
  EMLasso.lognet.cv(..., logdir = "./",
    saveTempResults = TRUE)
}
\arguments{
  \item{\dots}{passed on to
  \code{\link{EMLasso.1l.lognet.cv.param}}}

  \item{logdir}{path to folder where logfiles (and results)
  of each repeat are stored}

  \item{saveTempResults}{if \code{TRUE}, save the results
  of each (parallelized) repetition, so they can be
  reobtained if something goes wrong with the others.}
}
\value{
  a list holding \item{result }{list of
  \code{\link{cv.1l.emlasso.reduced}} objects per lambda}
  \item{params }{result of
  \code{\link{EMLasso.1l.lognet.cv.param}} call}
  \item{logdir }{as passed in}
}
\description{
  Run \code{\link{EMLasso.1l.lognet.cv}} for several lambda
  values through snowFall making use of
  \code{\link{run.parallel}}
}
\note{
  watch out! always reduced objects returned!
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlcvfit<-EMLasso.lognet.cv(aDfr.MD, y, lambda=c(0.05, 0.03), nrOfSamplesPerMDRow=7, verbosity=10)}
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  [PENDING]
}
\seealso{
  code{\link{cv.1l.emlasso.reduced}},
  \code{\link{EMLasso.1l.lognet.cv.param}},
  \code{\link{EMLasso.1l.lognet.cv}}
}
\keyword{EM}
\keyword{EMLasso}
\keyword{LASSO}
\keyword{logistic}

