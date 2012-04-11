\name{outcomeModelValidationReusables}
\alias{outcomeModelValidationReusables}
\alias{outcomeModelValidationReusables.default}
\alias{outcomeModelValidationReusables.lognet}
\title{Create reusable part of validation (for rejection sampling)}
\usage{
  outcomeModelValidationReusables(outcomeModel, ds, out,
    imputeDs2FitDsProperties, verbosity = 0)

  \method{outcomeModelValidationReusables}{default}(outcomeModel,
    ds, out, imputeDs2FitDsProperties, verbosity=0)

  \method{outcomeModelValidationReusables}{lognet}(outcomeModel,
    ds, out, imputeDs2FitDsProperties, verbosity=0)
}
\arguments{
  \item{outcomeModel}{outcome model (e.g.
  \code{\link{glmnet}})}

  \item{ds}{dataset for which validation needs to occur}

  \item{out}{outcomes for which validation needs to occur}

  \item{imputeDs2FitDsProperties}{see
  \code{\link{imputeDs2FitDs}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  a single logical (\code{TRUE} is convergence has happened
  or maxIt passed)

  the result of \code{basicCheckFunction}

  the result of \code{basicCheckFunction}
}
\description{
  Create reusable part of validation (for rejection
  sampling)
}
\note{
  the first row (initial estimates) and column (intercept)
  are skipped in the checks
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{EMLasso.1l}}

  \code{\link{EMLasso.1l}}
}
\keyword{outcome}
\keyword{rejection}
\keyword{sampling}
\keyword{validation}

