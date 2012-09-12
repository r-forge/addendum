\name{predictorModelSamplingReusables}
\alias{SamplingReusablesGLoMo}
\alias{SamplingReusablesGLoMo-class}
\alias{predictorModelSamplingReusables}
\alias{predictorModelSamplingReusables.GLoMo}
\title{Create reusable part of prediction for repeated sampling}
\usage{
  predictorModelSamplingReusables(predictorModel,
    iterCount, previousReusables = NULL, ds, verbosity = 0)

  \method{predictorModelSamplingReusables}{GLoMo}(predictorModel,
    iterCount, previousReusables=NULL, ds, verbosity=0)
}
\arguments{
  \item{predictorModel}{predictor model (e.g.
  \code{\link{GLoMo}})}

  \item{iterCount}{for the how manieth iteration is this
  the reusable part}

  \item{previousReusables}{result of this function for the
  previous iteration or \code{NULL} if there was none}

  \item{ds}{dataset for which prediction needs to occur}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  for the generic method: depending on the implementation.

  (in the \code{\link{GLoMo}} implementation) an object of
  class "SamplingReusablesGLoMo", i.e. a list with items:
  \enumerate{ \item \code{guidDataOfOriginalDfr}: return
  value of \code{\link{getGuidData}} \item
  \code{uidsPerRowOfOriginalDfr}: list holding for each row
  in \code{ds} the applicable uids in the
  \code{\link{GLoMo}} }
}
\description{
  Create reusable part of prediction for repeated sampling
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l}}
}
\keyword{guid}
\keyword{model}
\keyword{predictor}
\keyword{rejection}
\keyword{sampling}

