\name{differentModels}
\alias{differentModels}
\title{Find the different models selected during crossvalidation for EMLasso.lognet objects}
\usage{
  differentModels(model, ds = model$result[[1]]$dfr, out =
  model$result[[1]]$resp, nfolds = 10, dsconvprobs,
  reusableDatas, zeroThres = 1e-04, verbosity = 0)
}
\arguments{
  \item{model}{\code{\link{EMLasso.lognet}} object}

  \item{ds}{dataset on which to crossvalidate (default is
  the one used in \code{model})}

  \item{out}{outcome variable on which to crossvalidate
  (default is the one used in \code{model})}

  \item{nfolds}{number of folds for crossvalidation}

  \item{dsconvprobs}{see \code{\link{dfrConversionProbs}}
  and \code{\link{fit.lognet}}}

  \item{reusableDatas}{list of reusableData objects, one
  per \code{\link{GLoMo}} object, i.e. one per lambda. See
  also \code{\link{predict.GLoMo}}}

  \item{zeroThres}{how high does a value have to be (in
  absolute value) to be nonzero.}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  List with two items: \item{uniqueres}{list holding
  information on all unique models selected over all
  crossvalidations and lambdas}
  \item{perlamuniquecount}{number of unique models selected
  per lambda}
}
\description{
  Find the different models selected during crossvalidation
  for \code{\link{EMLasso.lognet}} objects
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
differentModels(iris.emlognet, verbosity=1)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{EMLasso}
\keyword{model}
\keyword{unique}

