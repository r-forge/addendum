\name{EMLasso.1l.param}
\alias{[.EMLasso.1l.param}
\alias{EMLasso.1l.param}
\alias{length.EMLasso.1l.param}
\title{Collect parameters to EMLasso.1l into one list}
\usage{
  EMLasso.1l.param(ds, out, lambda,
    nrOfSamplesPerMDRow = 10,
    rowsToUseForFit = seq(nrow(ds)),
    firstTimeCompleter = marginalCompleter(),
    imputeDs2FitDsProperties = normalImputationConversion(),
    fitPredictor = GLoMo, family = "binomial",
    convergenceChecker = convergenceCheckCreator(),
    postProcess = postProcessEMLasso1l, verbosity = 0)

  \method{[}{EMLasso.1l.param}(x, i)

  \method{length}{EMLasso.1l.param} (x)
}
\arguments{
  \item{ds}{see \code{\link{EMLasso.1l}}}

  \item{out}{see \code{\link{EMLasso.1l}}}

  \item{lambda}{see \code{\link{EMLasso.1l}}}

  \item{nrOfSamplesPerMDRow}{see \code{\link{EMLasso.1l}}}

  \item{rowsToUseForFit}{see \code{\link{EMLasso.1l}}}

  \item{firstTimeCompleter}{see \code{\link{EMLasso.1l}}}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{EMLasso.1l}}}

  \item{fitPredictor}{see \code{\link{EMLasso.1l}}}

  \item{family}{see \code{\link{EMLasso.1l}}}

  \item{convergenceChecker}{see \code{\link{EMLasso.1l}}}

  \item{postProcess}{see \code{\link{EMLasso.1l}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{x}{\code{EMLasso.1l.param} object}

  \item{i}{integer indexer}
}
\value{
  object of class "EMLasso.1l.param" having all these
  parameters as named members
}
\description{
  Collect parameters to \code{\link{EMLasso.1l}} into one
  list
}
\note{
  created for use in \code{\link{run.parallel}}
}
\examples{
aDfr<-generateTypicalIndependentDfr(numCat=10, numCnt=10, numObs=100, catProbs=rep(1/3,3),
rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=1)

outlins<- -mean(aDfr$cnt1)+aDfr$cnt1+2*(aDfr$cat1=="b")
outprobs<-expit(outlins)
y<-factor(sapply(outprobs, function(prob){sample(c("no", "yes"), 1, prob=c(1-prob,prob))}))

aDfr.MD<-randomNA(aDfr, 0.01, verbosity=1)
emlcvparm<-EMLasso.1l.param(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvparm<-EMLasso.1l.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
emlcvparm[1]
aDfr<-numdfr(generateTypicalIndependentDfr(100, 100, 150, doShuffle=FALSE))
y<-as.factor(rbinom(150, 1, 0.5))
emlcvparm<-EMLasso.1l.param(aDfr, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
length(emlcvparm)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{run.parallel}}, \code{\link{EMLasso.1l}}

  \code{\link{EMLasso.1l}}
}
\keyword{constructor}
\keyword{parameter}

