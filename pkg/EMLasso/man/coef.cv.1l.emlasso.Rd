\name{coef.cv.1l.emlasso}
\alias{coef.cv.1l.emlasso}
\title{Get coefficients of a "cv.1l.emlasso" object}
\usage{
  \method{coef}{cv.1l.emlasso} (object, foldname = "fold",
  itername = "iter", onlyFinal = FALSE,
  onlyEqualZeroPercent = 0, verbosity = 0, form =
  c("data.frame", "matrix", "sparse"), ...)
}
\arguments{
  \item{object}{\code{\link{cv.1l.emlasso}} object}

  \item{foldname}{name that can be safely given to a column
  holding the fold number. If \code{NULL}, no such column
  is added}

  \item{itername}{name that can be safely given to a column
  holding the iteration number If \code{NULL}, no such
  column is added}

  \item{onlyFinal}{if \code{TRUE}, then only the
  coefficients for the last iteration (normally
  convergence) are returned}

  \item{onlyEqualZeroPercent}{return only coefficients that
  are zero at most this fraction of the repetitions/folds}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{form}{return the results as either a "data.frame",
  "matrix" or "sparse".}

  \item{\dots}{passed on to
  \code{\link{coef.cvpart.emlasso}}}
}
\value{
  depending mostly on the parameter \code{form}, but
  typically: per fold/iteration a row, per coefficient
  (that is nonzero often enough) a column
}
\description{
  Get coefficients of a \code{\link{cv.1l.emlasso}} object
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
emlfit<-eml17$actualfits[[1]]$fitinfo
coef(emlfit, onlyFinal=TRUE)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{coefficient}

