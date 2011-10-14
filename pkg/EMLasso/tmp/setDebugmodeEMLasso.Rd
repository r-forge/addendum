\name{setDebugmodeEMLasso}
\alias{setDebugmodeEMLasso}
\title{Set tracing of all calls on/off}
\usage{
  setDebugmodeEMLasso(doDebug = TRUE)
}
\arguments{
  \item{doDebug}{\code{TRUE} to turn it on, \code{FALSE}
  for off}
}
\value{
  invisibly the old value
}
\description{
  If set to TRUE, some calls to a \code{\link{addendum}}
  specific implementation is traced.
}
\note{
  not effectively used in the package at this time
}
\examples{
setDebugmodeEMLasso()
setDebugmodeEMLasso(FALSE)
}
\author{
  Nick Sabbe \email{nick.sabbe@ugent.be}
}
\keyword{tracing}

