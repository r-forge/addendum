\name{crossValidate}
\alias{crossValidate}
\alias{crossValidate.EMLassoGLoMo}
\alias{cv.EMLasso.lognet}
\alias{cv.EMLassoGLoMo-class}
\alias{cv.MI.logreg}
\alias{cv.MI.logreg-class}
\alias{repeatedlyPredictOut}
\alias{repeatedPredictedProbAUC}
\title{Crossvalidate a model}
\usage{
  crossValidate(model, ..., verbosity = 0)

  \method{crossValidate}{EMLassoGLoMo}(model,
    ds=model$result[[1]]$ds, out=model$result[[1]]$out,
    wts=rep(1, nrow(ds)),
    imputeDs2FitDsProperties=model$imputeDs2FitDsProperties,
    imputations=10, ..., type.measure="auc",
    keepResultPerLambda=FALSE, nobs=1, unpenalized=FALSE,
    verbosity=0)

  repeatedlyPredictOut(glomo, ds, out, varsets, reps = 10,
    nfolds = 10,
    imputeDs2FitDsProperties = normalImputationConversion(),
    returnGroups = FALSE, returnCoefs = FALSE, ...,
    reusabledata, noScaling = TRUE, verbosity = 0)

  repeatedPredictedProbAUC(reppredprob, out, verbosity = 0,
    groups, onlyresult = TRUE, glmnetlike = TRUE)

  cv.MI.logreg(glomo, ds, out, useVarNames, reps,
    imputeDs2FitDsProperties = normalImputationConversion(),
    lambda, useAsGlmnetFit, ..., verbosity = 0)
}
\arguments{
  \item{model}{model fit}

  \item{\dots}{for flexibility in 'derived' implementation
  (passed on to \code{\link{collectImputationModels}} in
  \code{crossValidate.EMLassoGLoMo})}

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
  \code{crossValidate.EMLasso1l} are also returned in an
  extra item \code{resultPerLambda}}

  \item{nobs}{how many observations are simulated for each
  row with missing data}

  \item{unpenalized}{if \code{TRUE} (not the default) a
  simple regression model is fit with the selected
  variables}

  \item{glomo}{GLoMo model to predict from}

  \item{varsets}{list of character vectors holding the
  variables (names) to be checked}

  \item{reps}{number of predictions}

  \item{nfolds}{number of folds for crossvalidation}

  \item{returnGroups}{if \code{TRUE}, a list is returned
  with the normal result as its \code{result} item and a
  matrix holding the group assignment per repetition as the
  \code{groups} item.}

  \item{returnCoefs}{if \code{TRUE}, a list is returned
  with the normal result as its \code{result} item and a
  list of matrices holding the coefficient values per
  repetition and per fold as the \code{coefs} item.}

  \item{reusabledata}{optional premade result of
  \code{\link{reusableDataForGLoMoSampling}}}

  \item{noScaling}{if \code{TRUE} (the default) the data is
  not scaled upon conversion}

  \item{reppredprob}{one of the matrices as return by
  \code{repeatedlyPredictOut}}

  \item{groups}{vector/matrix of fold membership
  assignment. If nor present, 10 random groups are created}

  \item{onlyresult}{if \code{FALSE}, the return value holds
  (as list members) the the overall AUC and sd, but also
  the results per repetition}

  \item{glmnetlike}{if \code{TRUE},
  \code{\link{calcAUC.glmnet}} is used to calculate AUC and
  sd, otherwise \code{\link{calcAUC.Binary}} is used}

  \item{useVarNames}{names of columns to include in the
  model (character vector)}

  \item{lambda}{value to use as lambda in the return value
  (note: ignored for the rest)}

  \item{useAsGlmnetFit}{object that can be used for the
  \code{glmnet.fit} item in the return value}
}
\value{
  object that has as class: "cv." pasted before the class
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

  List of the same length as \code{varsets} (unless it was
  length 1, then the first object is simply returned). Each
  item is a matrix with one row for each row in \code{ds}
  and one column per \code{reps}, and holds the predicted
  probability in a crossvalidation.

  named vector of length 2, holding the "AUC" and the
  "AUCSD"

  object of classes "cv.MI.logreg", "cv.glmnet" and
  "cv.lognet". Has exactly the items of a
  \code{\link{cv.glmnet}} object
}
\description{
  Crossvalidate a model
}
\note{
  aids to generalize crossvalidation
}
\examples{
y<-rbinom(nrow(iris), 1, 0.5)
iris.cpy<-randomNA(iris, n=0.1)
iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,
	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
sfStop()
iris.cv.emlognet<-crossValidate(iris.emlognet, verbosity=2)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso}}, \code{\link{cv.glmnet}}
}
\keyword{crossvalidate}
\keyword{EMLasso}
\keyword{GLoMo}
\keyword{model}

