\name{crossValidate}
\alias{"cv.EMLasso.1l.lognet-class"}
\alias{"cv.EMLasso.lognet-class"}
\alias{crossValidate}
\alias{crossValidate.EMLasso.1l.lognet}
\alias{crossValidate.EMLasso.lognet}
\alias{cv.EMLasso.1l.lognet}
\alias{cv.EMLasso.lognet}
\alias{cv.MI.logreg}
\alias{repeatedlyPredictOut}
\alias{repeatedPredictedProbAUC}
\title{Crossvalidate a model}
\usage{
  crossValidate(model, ..., verbosity = 0)

  \method{crossValidate}{EMLasso.1l.lognet}(model,
  ds=model$dfr, out=model$resp, glomo=model$glomo,wts,
  dsconvprobs, needPredict=0, betweenColAndLevel="",
  type.measure="auc",..., verbosity=0)

  \method{crossValidate}{EMLasso.lognet}(model,
  ds=model$result[[1]]$dfr, out=model$result[[1]]$resp,
  wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0,
  betweenColAndLevel="",..., type.measure="auc",
  keepResultPerLambda=FALSE, useCombinedGLoMo=FALSE,
  verbosity=0)

  repeatedlyPredictOut(glomo, ds, out, varsets, reps = 10,
  nfolds = 10, dsconvprops = NULL, returnGroups = FALSE,
  ..., reusabledata, verbosity = 0)

  repeatedPredictedProbAUC(reppredprob, out, verbosity = 0,
  groups, onlyresult = TRUE, glmnetlike = TRUE)

  cv.MI.logreg(glomo, ds, out, useVarNames, reps,
  dsconvprops, lambda, useAsGlmnetFit, ..., verbosity = 0)
}
\arguments{
  \item{model}{model fit}

  \item{\dots}{for flexibility in 'derived' implementation}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{ds}{dataset with predictors}

  \item{out}{vector (binary factor) of outcomes}

  \item{glomo}{\code{\link{GLoMo}} object to use as
  predictor model}

  \item{wts}{vector of weights (defaults to equal weights
  for all rows)}

  \item{dsconvprobs}{see \code{\link{dfrConversionProbs}}}

  \item{needPredict}{If \code{> 0}, the number of rows that
  is predicted from the \code{GLoMo} in \code{model} for
  rows with missing data in \code{ds}. I \code{<0} then
  crossvalidation happens in a multiple imputation fashion
  (each time imputing only once, but doing so repeatedly)}

  \item{betweenColAndLevel}{see
  \code{\link{dfrConversionProbs}}}

  \item{type.measure}{see \code{\link{cv.glmnet}}}

  \item{keepResultPerLambda}{if \code{TRUE} (not the
  default), the individual results from the
  \code{crossValidate.EMLasso.1l.lognet} are also returned
  in an extra item \code{resultPerLambda}}

  \item{useCombinedGLoMo}{if \code{FALSE} (default), a
  distinct GLoMo is used for every lambda. Otherwise, the
  combined GLoMo is used for all lambdas.}

  \item{varsets}{list of character vectors holding the
  variables (names) to be checked}

  \item{reps}{how many times does imputation have to be
  repeated?}

  \item{nfolds}{number of folds for crossvalidation}

  \item{dsconvprops}{see \code{dsconvprobs} (need to work
  on universal and correct naming...)}

  \item{returnGroups}{if \code{TRUE}, a list is returned
  with the normal result as its \code{result} item and a
  matrix holding the group assignment per repetition as the
  \code{groups} item.}

  \item{reusabledata}{optional premade result of
  \code{\link{reusableDataForGLoMoSampling}}}

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
  object of type "cv.EMLasso.1l.lognet": \item{cv.logreg
  }{list of \code{\link{cv.1l.emlasso.reduced}} objects per
  lambda} \item{ds }{as passed in or reduced if predicted}
  \item{out }{as passed in or extended if predicted}
  \item{wts }{as passed in or extended if predicted}
  \item{fromLambda }{as passed in, the lambda that came
  from the original model}

  object of type "cv.EMLasso.lognet". This is mainly the
  same as a \code{\link{cv.glmnet}}. The added/altered
  items are: \item{glmnet.fit }{is now the model passed in,
  so of class "EMLasso.lognet", besides "glmnet"}
  \item{resultPerLambda }{list of "cv.EMLasso.1l.lognet"
  objects per lambda. Not present if
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
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}}

  \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}},
  \code{\link{cv.glmnet}}
}
\keyword{crossvalidate}
\keyword{EMLasso}
\keyword{GLoMo}
\keyword{model}

