\name{factorsToDummyVariables.numdfr}
\alias{factorsToDummyVariables.numdfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quickly create design matrix for simple cases
}
\description{
\code{\link{numdfr}} specific version of this function from \code{\link{addendum}}.
}
\usage{
\method{factorsToDummyVariables}{numdfr}(dfr, betweenColAndLevel = "", dfrConvData, verbosity=0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
\code{\link{numdfr}} object for which the design matrix must be made
}
  \item{betweenColAndLevel}{
If factors are present, what character should be between the original name and the value name?
}
  \item{dfrConvData}{
Prepared conversion info (return value of \code{dfrConversionProbs}). Can be
used (and is preferred) to avoid repeating calculations.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Ignored in standard implementations
}
}
\value{
Matrix with a row for every observation and a column for every numerical variable/ level of factor.

}
\author{
Nick Sabbe
}
\note{
Typical use is for \code{\link[glmnet]{glmnet}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NumDfr}}, \code{\link{addendum}}, \code{\link{factorsToDummyVariables}}
}
\examples{
iris.nd<-numdfr(iris)
factorsToDummyVariables(iris.nd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ factor }
\keyword{ design }
\keyword{ numdfr }% __ONLY ONE__ keyword per line
