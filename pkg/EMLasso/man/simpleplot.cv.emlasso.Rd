\name{simpleplot.cv.emlasso}
\alias{simpleplot.cv.emlasso}
\title{simpleplot implementation for class cv.emlasso}
\usage{
  \method{simpleplot}{cv.emlasso} (object, beta.type =
  NULL, xvar = c("norm", "lambda", "dev"), label = FALSE,
  ..., verbosity = 0)
}
\arguments{
  \item{object}{\code{\link{cv.emlasso}} object}

  \item{beta.type}{see \code{type} parameter of
  \code{\link{getBeta}}}

  \item{xvar}{see \code{\link{simpleplot}}}

  \item{label}{see \code{\link{simpleplot}}}

  \item{\dots}{see \code{\link{simpleplot}}}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  see \code{\link{simpleplot}}
}
\description{
  \code{\link{simpleplot}} implementation for class
  \code{\link{cv.emlasso}}
}
\note{
  This makes sure \code{\link{plotex}} works properly
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{emlcvfit<-EMLasso.lognet.cv(aDfr.MD, y, lambda=c(0.05, 0.03), nrOfSamplesPerMDRow=7, verbosity=10)}
emlcvfitred<-reduce(emlcvfit)
simpleplot(emlcvfitred)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\seealso{
  \code{\link{simpleplot}}, \code{\link{plotex}}
}
\keyword{simpleplot}

