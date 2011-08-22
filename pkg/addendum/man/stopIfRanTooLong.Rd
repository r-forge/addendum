\name{stopIfRanTooLong}
\alias{stopIfRanTooLong}
\alias{getTimeOut}
\alias{setTimeOut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interrupt execution if part of the process took too long.
}
\description{
Avoid repeatedly doing lengthy calculations if you know the repeated part takes too long anyway.
}
\usage{
stopIfRanTooLong(expr, to = getTimeOut(), verbosity = 0)
getTimeOut(toname="usedTimeOut")
setTimeOut(to, toname="usedTimeOut")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{
expression to repeat
}
  \item{to}{
maximum duration allowed
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
  \item{toname}{
get/set this variable in the global envrionment
}

}
\value{
return value of \code{expr}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
orgTo<-getTimeOut()
setTimeOut(0.5)
res<-try(for(i in 1:10) {stopIfRanTooLong({Sys.sleep(1)})})
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~timeout }
