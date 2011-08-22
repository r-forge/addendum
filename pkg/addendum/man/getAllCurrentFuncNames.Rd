\name{getAllCurrentFuncNames}
\alias{getAllCurrentFuncNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
All function names in the global environment
}
\description{
Finds the names of all functions in \code{.GlobalEnv}
}
\usage{
getAllCurrentFuncNames(dropSourceGeneratingFuncs = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dropSourceGeneratingFuncs}{
If TRUE, some functions aimed at generating code for the workspace are excluded.
}
}
\value{
vector containing all the names.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getAllCurrentFuncNames()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~function }
\keyword{ ~environment }
