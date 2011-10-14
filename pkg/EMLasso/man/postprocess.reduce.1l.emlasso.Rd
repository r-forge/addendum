\name{postprocess.reduce.1l.emlasso}
\alias{postprocess.reduce.1l.emlasso}
\title{Postprocessing function for reduction - after save of object, before return}
\usage{
  postprocess.reduce.1l.emlasso(eml, cparam, i, verbosity)
}
\arguments{
  \item{eml}{\code{\link{cv.1l.emlasso}} object}

  \item{cparam}{\code{\link{EMLasso.1l.lognet.cv.param}}
  object}

  \item{i}{indexer for the \code{i}th lambda}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  see \code{\link{reduce.cv.1l.emlasso}}
}
\description{
  Postprocessing function for reduction - after save of
  object, before return
}
\note{
  simply calls \code{\link{reduce.cv.1l.emlasso}}
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{reduce}

