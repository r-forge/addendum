\name{collectImputationModels}
\alias{EMLassoGLoMoImputationData}
\alias{EMLassoGLoMoImputationData-class}
\alias{collectImputationModels}
\alias{collectImputationModels.EMLassoGLoMo}
\alias{predict.EMLassoGLoMoImputationData}
\title{Collect data from a model, needed for multiple imputation based on this model}
\usage{
  collectImputationModels(model, ds = model$result[[1]]$ds,
    ..., verbosity = 0)

  \method{collectImputationModels}{EMLassoGLoMo}(model,
    ds=model$result[[1]]$ds, useCombinedGLoMo=TRUE, ...,
    verbosity=0)

  \method{predict}{EMLassoGLoMoImputationData}(object,
    newdata, out, wts=rep(1, nrow(newdata)),
    type.measure="auc", ..., verbosity=0)
}
\arguments{
  \item{model}{model fit}

  \item{ds}{dataset for which imputation will need to
  happen}

  \item{\dots}{implementation dependent. e.g. for
  \code{\link{EMLassoGLoMo}} should be a list containig an
  item \code{useCombinedGLoMo} (will default to
  \code{TRUE}); for
  \code{predict.EMLassoGLoMoImputationData}: passed on to
  \code{\link{cv.glmnet}}. Note: the ones implied by
  \code{object} are not supported! e.g. \code{family} and
  \code{standardize} cannot be overridden}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{useCombinedGLoMo}{if \code{FALSE} (not the
  default), a distinct GLoMo is used for every lambda.
  Otherwise, the combined GLoMo (see
  \code{\link{combineGLoMos}}) is used for all lambdas.}

  \item{object}{"EMLassoGLoMoImputationData" that holds the
  information to perform imputations}

  \item{newdata}{dataset for which imputation needs to
  occur}

  \item{out}{outcomes that will be used for evaluating the
  models}

  \item{wts}{weights per observation (defaults to equal
  weights for all observations)}

  \item{type.measure}{see \code{\link{cv.glmnet}}}
}
\value{
  \code{collectImputationModels.EMLassoGLoMo} will return
  an object of class "EMLassoGLoMoImputationData".  This is
  a list with items: \item{imputationParams }{the \dots
  passed along} \item{useCombinedGLoMo }{as passed along}
  \item{combinedGLoMo }{if relevant
  (\code{useCombinedGLoMo==TRUE}): the combined
  \code{\link{GLoMo}}} \item{glomolist }{if relevant
  (\code{useCombinedGLoMo==FALSE}): list containing the
  \code{\link{GLoMo}} to be used for each lambda}
  \item{reusableData }{either a single object of class
  "ReusableDataForGLoMoSampling" (see
  \code{\link{reusableDataForGLoMoSampling}}), or a list
  with one of them per lambda} \item{lambda }{taken from
  \code{model}} \item{family }{taken from \code{model}}
  \item{imputeDs2FitDsProperties }{taken from \code{model}}
}
\description{
  Collect data from a model, needed for multiple imputation
  based on this model
}
\note{
  aids to generalize crossvalidation
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso}}, \code{\link{cv.glmnet}}

  \code{\link{EMLasso}}, \code{\link{cv.glmnet}}
}
\keyword{EMLasso}
\keyword{GLoMo}
\keyword{crossvalidate}
\keyword{imputation}
\keyword{model}

