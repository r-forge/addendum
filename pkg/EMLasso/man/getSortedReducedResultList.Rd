\name{getSortedReducedResultList}
\alias{getSortedReducedResultList}
\title{Reduce several cv.1l.emlasso objects (saved or not), sort and return as a new list}
\usage{
  getSortedReducedResultList(object, orgdfr, ...,
    verbosity = 0, splitPatternLastPart = "/")
}
\arguments{
  \item{object}{list of \code{\link{cv.1l.emlasso}}
  objects, or path containing saved such objects (per
  file), or path + filename pattern}

  \item{orgdfr}{original dataset used to fit the EMLassos}

  \item{\dots}{passed on to
  \code{\link{reduce.cv.1l.emlasso}} for each object}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{splitPatternLastPart}{which character(s) can be
  used to distinguish the path from the filename pattern in
  \code{object} (if it is a character)}
}
\value{
  list, sorted by lambda, of
  \code{\link{cv.1l.emlasso.reduced}} objects, one for
  every original object. The list is of class
  "reducedResultList"
}
\description{
  Reduce several cv.1l.emlasso objects (saved or not), sort
  and return as a new list
}
\note{
  These kinds of objects are saved during
  \code{\link{EMLasso.lognet.cv}}
}
\examples{
data(emlcvfit, package="EMLasso")
\dontrun{eml17<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)
eml20<-EMLasso.1l.lognet.cv(aDfr.MD, y, lambda=0.03, nrOfSamplesPerMDRow=7, verbosity=10)}
redlist<-getSortedReducedResultList(list(eml17, eml20), verbosity=10)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{list}
\keyword{reduce}

