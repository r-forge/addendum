\name{fullDataEMLassoAndGLoMo}
\alias{EMLasso.lognet}
\alias{EMLasso.lognet-class}
\alias{fullDataEMLassoAndGLoMo}
\title{Fit EMLasso to a complete dataset, creating a GLoMo for each lambda}
\usage{
  fullDataEMLassoAndGLoMo(ds, out, dsconvprobs, lambdas,
    betweenColAndLevel = "", showPlot = TRUE,
    type.measure = "auc", repsNeededForFirstOccurrence = 3,
    minNumHigher = 20, minNumLower = 20, maxNumLower = 30,
    ..., verbosity = 0, logdir = "./",
    saveTempResults = TRUE)
}
\arguments{
  \item{ds}{dataset with predictors}

  \item{out}{vector (binary factor) of outcomes}

  \item{dsconvprobs}{see \code{\link{dfrConversionProbs}}}

  \item{lambdas}{see \code{\link{dfrConversionProbs}}}

  \item{showPlot}{see
  \code{\link{findReasonableLambdaHelper}} (only relevant
  if no \code{lambdas} passed)}

  \item{type.measure}{see
  \code{\link{findReasonableLambdaHelper}} (only relevant
  if no \code{lambdas} passed)}

  \item{repsNeededForFirstOccurrence}{see
  \code{\link{findReasonableLambdaHelper}} (only relevant
  if no \code{lambdas} passed)}

  \item{minNumHigher}{see
  \code{\link{findReasonableLambdaHelper}} (only relevant
  if no \code{lambdas} passed)}

  \item{minNumLower}{see
  \code{\link{findReasonableLambdaHelper}} (only relevant
  if no \code{lambdas} passed)}

  \item{maxNumLower}{see
  \code{\link{findReasonableLambdaHelper}} (only relevant
  if no \code{lambdas} passed)}

  \item{betweenColAndLevel}{see
  \code{\link{dfrConversionProbs}}}

  \item{\dots}{passed on to
  \code{\link{EMLasso.1l.lognet}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{logdir}{path to folder where logfiles (and results)
  of each repeat are stored}

  \item{saveTempResults}{if \code{TRUE}, save the results
  of each (parallelized)}
}
\value{
  an object of class "EMLasso.lognet", "glmnet" and
  "lognet". The items added to the \code{\link{glmnet}}
  members are: \item{result }{list of
  \code{\link{cv.1l.emlasso}} objects per lambda}
  \item{params }{\code{\link{EMLasso.1l.lognet.param}}
  parameters passed in} \item{logdir }{directory where
  logging/saving occurred} \item{combinedGLoMo
  }{\code{\link{GLoMo}} object: the combination (through
  \code{\link{combineGLoMos}}) of the \code{\link{GLoMo}}s
  for each lambda}
}
\description{
  Fit EMLasso to a complete dataset, creating a GLoMo for
  each lambda
}
\note{
  If lambdas is not passed along or is \code{NULL}, a set
  of lambdas is used by utilizing
  \code{\link{findReasonableLambdaHelper}}
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
require(addendum)
require(NumDfr)
require(GLoMo)
require(snowfall)
require(EMLasso)
sfInit(parallel = FALSE, cpus = 1)
sfLibrary(addendum)
sfLibrary(NumDfr)
sfLibrary(GLoMo)
sfLibrary(EMLasso)
iris.cpy<-randomNA(iris, n=0.1)
iris.emlognet<-fullDataEMLassoAndGLoMo(ds=numdfr(iris.cpy), out=y,
	showPlot=FALSE, type.measure="auc", lambdas=c(0.03,0.002,0.0003),
	nrOfSamplesPerMDRow=10, verbosity=2,
	minIt=5, maxIt=150, precalcGuidData=FALSE)
sfStop()
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l.lognet}}
}
\keyword{EMLasso}
\keyword{GLoMo}

