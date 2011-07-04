\name{currentFuncDependencyTree}
\alias{currentFuncDependencyTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find dependency tree from all loaded custom functions
}
\description{
Searches sources of all loaded custom functions (see \code{\link{getAllCurrentFuncNames}}) for calls to generate a dependency tree
}
\usage{
currentFuncDependencyTree(aggressive = TRUE, verbosity=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aggressive}{
If TRUE, only looks for function names followed by a bracket.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
}
\details{
Not full-proof: only makes use of the source for each function!

Also it will not respond well to recursion.
}
\value{
Dataframe with two columns
  \item{mainfunc }{Contains each function that calls at least one other function}
  \item{dependsonfunc }{For each \code{mainfunc} the functions it calls}
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getAllCurrentFuncNames}}
}
\examples{
currentFuncDependencyTree(TRUE, verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~dependency }
\keyword{ ~tree }
\keyword{ ~function }
