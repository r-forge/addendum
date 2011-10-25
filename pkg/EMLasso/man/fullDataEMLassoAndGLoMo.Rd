\name{fullDataEMLassoAndGLoMo}
\alias{fullDataEMLassoAndGLoMo}
\title{Fit EMLasso to a complete dataset, creating a GLoMo for each lambda}
\usage{
  fullDataEMLassoAndGLoMo(ds, out, dsconvprobs, lambdas,
  betweenColAndLevel = "", ..., verbosity = 0)
}
\arguments{
  \item{ds}{dataset with predictors}

  \item{out}{vector (binary factor) of outcomes}

  \item{dsconvprobs}{see \code{\link{dfrConversionProbs}}}

  \item{lambdas}{see \code{\link{dfrConversionProbs}}}

  \item{betweenColAndLevel}{see
  \code{\link{dfrConversionProbs}}}

  \item{\dots}{passed on to
  \code{\link{findReasonableLambdaHelper}} if needed.}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  to be looked into
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
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l.lognet.cv}}
}
\keyword{EMLasso}
\keyword{GLoMo}

