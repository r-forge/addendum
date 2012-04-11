\name{postProcessEMLasso1l}
\alias{postProcessEMLasso1l}
\alias{postProcessEMLasso1l.default}
\alias{postProcessEMLasso1l.lognet}
\title{Postprocess EMLasso for 1 lambda result (e.g. add extra members or change class)}
\usage{
  postProcessEMLasso1l(outcomeModel, retval, lastData,
    imputeDs2FitDsProperties, reusableForSampling,
    reusableForvalidation, verbosity = 0)

  \method{postProcessEMLasso1l}{default}(outcomeModel,
    retval, lastData, imputeDs2FitDsProperties,
    reusableForSampling, reusableForvalidation,
    verbosity=0)

  \method{postProcessEMLasso1l}{lognet}(outcomeModel,
    retval, lastData, imputeDs2FitDsProperties,
    reusableForSampling, reusableForvalidation,
    verbosity=0)
}
\arguments{
  \item{outcomeModel}{the final outcome model will be
  passed in here (e.g. \code{\link{glmnet}} object)}

  \item{retval}{the return value of
  \code{\link{EMLasso.1l}} to be postprocessed}

  \item{lastData}{dataset (completed) from the last EM
  iteration}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}}}

  \item{reusableForSampling}{Which of the rows of dfr/resp
  can be used for fitting the LASSO}

  \item{reusableForvalidation}{minimum number of iterations
  before convergence is possible}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  Typically the passed along \code{retval} object with
  optionally some more items/class/attributes
}
\description{
  Postprocess EMLasso for 1 lambda result (e.g. add extra
  members or change class)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l}}
}
\keyword{EMLasso}
\keyword{postprocess}

