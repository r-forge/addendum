\name{listFuncsOfType}
\alias{listFuncsOfType}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find all functions following a name pattern in a given environment
}
\description{
Searches an environment for objects that follow the pattern (e.g. \code{"do*"}) and that are functions.
}
\usage{
listFuncsOfType(tp, envir = globalenv())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tp}{
Pattern for the names of the functions. Wildcards can be used (it is used as the \code{pattern} parameter to \code{\link{ls}}).
}
  \item{envir}{
Environment to look for functions. Defaults to \code{globalenv()}
}
}
\value{
vector holding the names of the functions.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
doSomething<-function(){cat("whatever\n")}
dontDoSomething<-function(){cat("not whatever\n")}
sneakyBastard<-function(){doSomething()}
listFuncsOfType("do*")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~function }
