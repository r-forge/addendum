\name{getAllFuncsInDependencyOrder}
\alias{getAllFuncsInDependencyOrder}
\alias{printFuncs}
\alias{printFuncSource}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds the names of all functions in the global environment in order of dependency
}
\description{
Function \code{\link{currentFuncDependencyTree}} returns all functions and their dependencies
simply as a data.frame. This function uses that result to try and order the functions.
}
\usage{
getAllFuncsInDependencyOrder()
}
%- maybe also 'usage' for other objects documented here.
\details{
The goal of this function is to be able to check which functions might need to be
rechecked if a given function changes.

\code{printFuncs} is a function that prints the kind of data.frame that is returned by
\code{getAllFuncsInDependencyOrder}. It uses the helper function \code{printFuncSource} behind the scenes.
}
\value{
A data.frame with a row for each function in the \code{.GlobalEnv} and two columns:
  \item{func }{Name of the function}
  \item{print }{always TRUE, present for backwards compatibility}
}
\author{
Nick Sabbe
}
\note{
Warns against mutual references, but not fully tested.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{currentFuncDependencyTree}}
}
\examples{
getAllFuncsInDependencyOrder()$func
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~function }
\keyword{ ~dependency }
\keyword{ ~tree }
