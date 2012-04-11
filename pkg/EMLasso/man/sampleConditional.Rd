\name{sampleConditional}
\alias{acceptOrRejectFunction}
\alias{acceptOrRejectFunction.default}
\alias{acceptOrRejectFunction.lognet}
\alias{sampleConditional}
\alias{sampleConditional.GLoMo}
\alias{sampledConditionally}
\alias{sampledConditionally-class}
\alias{sampledConditionallyGLomo}
\alias{sampledConditionallyGLomo-class}
\title{Complete a dataset conditional on a model and the outcomes}
\usage{
  sampleConditional(predictorModel, outcomeModel,
    nrOfSamplesPerMDRow, ds, out, rowsToUseForFit,
    imputeDs2FitDsProperties, reusableForSampling,
    reusableForvalidation, verbosity = 0)

  \method{sampleConditional}{GLoMo}(predictorModel,
    outcomeModel, nrOfSamplesPerMDRow, ds, out,
    rowsToUseForFit, imputeDs2FitDsProperties,
    reusableForSampling, reusableForvalidation,
    verbosity=0)

  acceptOrRejectFunction(outcomeModel)

  \method{acceptOrRejectFunction}{default}(outcomeModel)

  \method{acceptOrRejectFunction}{lognet}(outcomeModel)
}
\arguments{
  \item{predictorModel}{model binding the predictors (e.g.
  \code{\link{GLoMo}})}

  \item{outcomeModel}{outcome model (linking the outcomes
  to the predictors) (e.g. \code{\link{glmnet}} object)}

  \item{nrOfSamplesPerMDRow}{number times each row with
  missing data will be completed}

  \item{ds}{dataset to be completed}

  \item{out}{outcomes}

  \item{rowsToUseForFit}{which of the rows in \code{ds} can
  be used for fitting the outcome model}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}}}

  \item{reusableForSampling}{Which of the rows of dfr/out
  can be used for fitting the LASSO}

  \item{reusableForvalidation}{minimum number of iterations
  before convergence is possible}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  object of classes "sampledConditionallyGLomo" and
  "sampledConditionally", which is a list with items:
  \enumerate{ \item \code{ds}: completed dataset \item
  \code{out}: outcome per row of \code{ds} (the one in this
  list) \item \code{weights}: weight per row in \code{ds}
  (the one in this list) \item \code{orgri}: row index
  within \code{ds} (the parameter of the function) of each
  row within \code{ds} (the one in this list) \item
  \code{useForFit}: boolean indicator for each row of
  \code{ds} (the one in this list) that is \code{TRUE} if
  this row is to be used for validation \item
  \code{useOut}: outcomes, but only for the rows of
  \code{ds} that can be used for validation }

  \code{sampleConditional} returns a function with the same
  signature and goal as
  \code{\link{validateFunction.lognet}}
}
\description{
  Complete a dataset conditional on a model and the
  outcomes
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}

  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l}}

  \code{\link{EMLasso.1l}}

  \code{\link{EMLasso.1l}}

  \code{\link{EMLasso.1l}}
}
\keyword{EMLasso}
\keyword{postprocess}

