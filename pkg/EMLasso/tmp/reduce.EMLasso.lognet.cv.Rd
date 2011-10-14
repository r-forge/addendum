\name{reduce.EMLasso.lognet.cv}
\alias{reduce.EMLasso.lognet.cv}
\title{Reduce memory footprint of a list of \code{\link{cv.1l.emlasso}} objects and recalculates some criteria}
\usage{
  \method{reduce}{cv.emlasso} (object, orgdfr, orgresp,
  samplesForFinalFit = 10, ..., verbosity = 0,
  splitPatternLastPart = "/", keepReducedResultList =
  FALSE)
}
\arguments{
  \item{object}{list of \code{\link{cv.1l.emlasso}} objects
  or object of class "reducedResultList"}

  \item{orgdfr}{original dataset used. If missing, the
  first one found is used}

  \item{orgresp}{original outcome vector used. If missing,
  the first one found is used}

  \item{samplesForFinalFit}{how many samples must be used
  in the final GLoMo to make a dataset for the final refit}

  \item{\dots}{passed on to
  \code{\link{getSortedReducedResultList}} if needed}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{splitPatternLastPart}{see
  \code{\link{getSortedReducedResultList}}}

  \item{keepReducedResultList}{if \code{TRUE}, the
  resulting "reducedResultList" object is kept as an item
  of the return value}
}
\value{
  object of class \code{\link{cv.emlasso}} (and
  \code{\link{cv.glmnet}}, to be able to abuse some of
  their code)
}
\description{
  Reduce memory footprint of a list of
  \code{\link{cv.1l.emlasso}} objects and recalculates some
  criteria
}
\note{
  %% ~~further notes~~
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\references{
  %% ~put references to the literature/web site here ~
}
\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{reduce}

