\name{simplyValidate}
\alias{simplyValidate}
\alias{simplyValidate.EMLassoGLoMo}
\alias{sv.EMLasso.lognet}
\alias{sv.EMLassoGLoMo-class}
\title{Validate a model (to its original data)}
\usage{
  simplyValidate(model, ..., verbosity = 0)

  \method{simplyValidate}{EMLassoGLoMo}(model,
    ds=model$result[[1]]$ds, out=model$result[[1]]$out,
    wts=rep(1, nrow(ds)),
    imputeDs2FitDsProperties=model$imputeDs2FitDsProperties,
    imputations=10, ..., type.measure="auc",
    keepResultPerLambda=FALSE, nobs=1, unpenalized=FALSE,
    verbosity=0)
}
\arguments{
  \item{model}{model fit}

  \item{\dots}{for flexibility in 'derived' implementation}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{ds}{dataset with predictors}

  \item{out}{vector (binary factor) of outcomes}

  \item{wts}{vector of weights (defaults to equal weights
  for all rows)}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}} object that will provide the
  conversion from imputed dataset to one that is ready for
  fitting the predictor model}

  \item{imputations}{Number of multiple imputations on the
  complete dataset (defaults to 10)}

  \item{type.measure}{see \code{\link{cv.glmnet}}}

  \item{keepResultPerLambda}{if \code{TRUE} (not the
  default), the individual results from the
  \code{simplyValidate.EMLasso1l} are also returned in an
  extra item \code{resultPerLambda}}

  \item{nobs}{how many observations are simulated for each
  row with missing data}

  \item{unpenalized}{if \code{TRUE} (not the default) a
  simple regression model is fit with the selected
  variables}
}
\value{
  object that has as class: "sv." pasted before the class
  of \code{model}. Normally, \code{model} will will be the
  return value of \code{\link{EMLasso}}, so this result is
  mainly the same as a \code{\link{cv.glmnet}}. The
  added/altered items are: \item{glmnet.fit}{is now the
  \code{model} passed in, so has more classes besides
  "glmnet" (e.g. "EMLasso")} \item{resultPerLambda }{matrix
  with one column per imputation. The top rows are the
  estimates for the criterion per lambda, below that are
  their SD estimates. Not present if
  \code{keepResultPerLambda=FALSE}}
}
\description{
  Validate a model (to its original data)
}
\note{
  aids to generalize validation
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
iris.cpy<-randomNA(iris, n=0.1)
iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,
	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
sfStop()
iris.sv.emlognet<-simplyValidate(iris.emlognet, verbosity=2)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso}}, \code{\link{cv.glmnet}}
}
\keyword{EMLasso}
\keyword{GLoMo}
\keyword{model}
\keyword{validate}

