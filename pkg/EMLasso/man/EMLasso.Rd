\name{EMLasso}
\alias{EMLasso}
\alias{EMLasso-class}
\alias{EMLasso.glmnet}
\alias{EMLasso.glmnet-class}
\alias{EMLasso.lognet}
\alias{EMLasso.lognet-class}
\title{Fit EMLasso to a complete dataset, creating a GLoMo for each lambda}
\usage{
  EMLasso(ds, out,
    family = c("gaussian", "binomial", "poisson", "multinomial", "cox"),
    lambdas = smartLambdaRetriever(),
    imputeDs2FitDsProperties = normalImputationConversion(),
    ..., verbosity = 0, logdir = "./",
    saveTempResults = TRUE,
    postProcessAll = postProcessEMLasso)
}
\arguments{
  \item{ds}{dataset with predictors}

  \item{out}{vector (binary factor) of outcomes}

  \item{family}{see \code{\link{glmnet}}}

  \item{lambdas}{see \code{\link{dfrConversionProps}}}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} object that will provide the
  conversion from imputed dataset to one that is ready for
  fitting the predictor model}

  \item{\dots}{passed on to \code{\link{EMLasso.1l}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{logdir}{path to folder where logfiles (and results)
  of each repeat are stored}

  \item{saveTempResults}{if \code{TRUE}, save the results
  of each (parallelized)}

  \item{postProcessAll}{function, like
  \code{\link{postProcessEMLasso}} (its default) and of
  that signature, to do some extra work before returning
  the result of \code{EMLasso}}
}
\value{
  an object of class "EMLasso", "glmnet" and some other
  classes based on the class of the outcome model. The
  items added to the \code{\link{glmnet}} members are:
  \item{result }{list of \code{\link{EMLasso1l}} objects
  per lambda} \item{params }{\code{\link{EMLasso.1l.param}}
  parameters passed in} \item{logdir }{directory where
  logging/saving occurred} \item{combinedGLoMo
  }{\code{\link{GLoMo}} object: the combination (through
  \code{\link{combineGLoMos}}) of the \code{\link{GLoMo}}s
  for each lambda (note: this item is added by
  \code{\link{postProcessEMLasso}}, so it may not be
  present if another \code{postProcessAll} is used)}
  \item{imputeDs2FitDsProperties}{as passed in, but first
  fed to \code{\link{imputeDs2FitDsProps}}}
  \item{family}{as passed in}
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
iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y, family="binomial",
	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
sfStop()
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l}}
}
\keyword{EMLasso}
\keyword{GLoMo}

