\name{postProcessEMLasso}
\alias{EMLassoGLoMo}
\alias{EMLassoGLoMo-class}
\alias{postProcessEMLasso}
\alias{postProcessEMLasso.default}
\alias{postProcessEMLasso.GLoMo}
\title{Postprocess EMLasso (e.g. add extra members or change class)}
\usage{
  postProcessEMLasso(predictorModel, retval, verbosity = 0)

  \method{postProcessEMLasso}{GLoMo}(predictorModel,
    retval, verbosity=0)

  \method{postProcessEMLasso}{default}(predictorModel,
    retval, verbosity=0)
}
\arguments{
  \item{predictorModel}{the final predictor model will be
  passed in here (e.g. \code{\link{GLoMo}} object)}

  \item{retval}{the return value of
  \code{\link{EMLasso.1l}} to be postprocessed}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  Typically the passed along \code{retval} object with
  optionally some more items/class/attributes
}
\description{
  Postprocess EMLasso (e.g. add extra members or change
  class)
}
\details{
  This function is (typically) called at the end of
  \code{\link{EMLasso}} and may alter the \code{class} of
  the end result and/or add some more items to the list. In
  the case of the \code{\link{GLoMo}} implementation, it
  adds the classnames extended with "GLoMo" and provides
  the \code{combinedGLoMo} item (see
  \code{\link{combineGLoMos}})
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l}}
}
\keyword{EMLasso}
\keyword{postprocess}

