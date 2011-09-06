\name{setDebugmodeAddendum}
\alias{setDebugmodeAddendum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set tracing of all calls on/off
}
\description{
If set to TRUE, some calls to a \code{\link{addendum}} specific implementation is traced.
}
\usage{
setDebugmodeAddendum(doDebug = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doDebug}{
if \code{TRUE}, set on, else off.
}
}
\value{
returns the previous value (a boolean)
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
setDebugmodeAddendum(FALSE)
setDebugmodeAddendum(TRUE)
setDebugmodeAddendum(FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ debug }
\keyword{ addendum }% __ONLY ONE__ keyword per line
