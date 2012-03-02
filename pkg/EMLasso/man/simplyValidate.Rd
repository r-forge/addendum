\name{simplyValidate}
\alias{simplyValidate}
\alias{simplyValidate.EMLasso.1l.lognet}
\alias{simplyValidate.EMLasso.lognet}
\alias{sv.EMLasso.1l.lognet}
\alias{sv.EMLasso.1l.lognet-class}
\alias{sv.EMLasso.lognet}
\alias{sv.EMLasso.lognet-class}
\title{Validate a model (to its original data)}
\usage{
  simplyValidate(model, ..., verbosity = 0)

  \method{simplyValidate}{EMLasso.1l.lognet}(model,
    ds=model$dfr, out=model$resp, wts, dsconvprobs,
    needPredict=0, betweenColAndLevel="",
    type.measure="auc", ..., verbosity=0)

  \method{simplyValidate}{EMLasso.lognet}(model,
    ds=model$result[[1]]$dfr, out=model$result[[1]]$resp,
    wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0,
    betweenColAndLevel="",..., type.measure="auc",
    keepResultPerLambda=FALSE, verbosity=0)
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

  \item{dsconvprobs}{see \code{\link{dfrConversionProbs}}}

  \item{needPredict}{If \code{> 0}, the number of rows that
  is predicted from the \code{GLoMo} in \code{model} for
  rows with missing data in \code{ds}}

  \item{betweenColAndLevel}{see
  \code{\link{dfrConversionProbs}}}

  \item{type.measure}{see \code{\link{cv.glmnet}} - for
  now, only "auc"" is supported}

  \item{keepResultPerLambda}{if \code{TRUE} (not the
  default), the individual results from the
  \code{simplyValidate.EMLasso.1l.lognet} are also returned
  in an extra item \code{resultPerLambda}}
}
\value{
  object of type "sv.EMLasso.1l.lognet": \item{sv.logreg
  }{\code{\link{cv.glmnet}}-like objects} \item{ds }{as
  passed in or reduced if predicted} \item{out }{as passed
  in or extended if predicted} \item{wts }{as passed in or
  extended if predicted} \item{fromLambda }{as passed in,
  the lambda that came from the original model}

  object of type "sv.EMLasso.lognet". This is mainly the
  same as a \code{\link{cv.glmnet}}. The added/altered
  items are: \item{glmnet.fit }{is now the model passed in,
  so of class "EMLasso.lognet", besides "glmnet"}
  \item{resultPerLambda }{list of "sv.EMLasso.1l.lognet"
  objects per lambda. Not present if
  \code{keepResultPerLambda=FALSE}}
}
\description{
  Validate a model (to its original data)
}
\note{
  aids to generalize validation
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}}

  \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}},
  \code{\link{cv.glmnet}}
}
\keyword{EMLasso}
\keyword{GLoMo}
\keyword{model}
\keyword{validate}

