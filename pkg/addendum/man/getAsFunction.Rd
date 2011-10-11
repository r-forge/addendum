\name{getAsFunction}
\alias{getAsFunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets function by name (if it wasn't already a function)
}
\description{
Gets function by name (if it wasn't already a function)
}
\usage{
getAsFunction(fnameOrFunction, returnIfNotFound=NULL, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fnameOrFunction}{
Either a function (in which case this is also the return value), or a character 
string that holds the name of the function to be returned
}
  \item{returnIfNotFound}{
If \code{fnameOrFunction} wasn't a function, and the function was not found, this
is what is returned (defaults to \code{NULL})
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug 
information is displayed (note: in R for Windows, turn off buffered output)}
}
\value{
Either a function (if successful) or \code{fnameOrFunction}
}
\author{
Nick Sabbe \email{nick.sabbe@ugent.be}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getAsFunction(getAsFunction, verbosity=1)
getAsFunction("getAsFunction", verbosity=1)
getAsFunction("thisFunctionProbablyDoesntExist", returnIfNotFound=function(x){return(x)}, verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ function }
\keyword{ name }
