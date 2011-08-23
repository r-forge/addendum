\name{setDebugmode}
\alias{setDebugmode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set tracing of all calls on/off
}
\description{
If set to TRUE, every call to a \code{\link{numdfr}} specific implementation is traced.
}
\usage{
setDebugmode(doDebug = TRUE)
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
iris.nd<-numdfr(iris)
setDebugmode(FALSE)
length(iris.nd)
setDebugmode(TRUE)
length(iris.nd)
setDebugmode(FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ debug }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
