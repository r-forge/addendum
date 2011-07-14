\name{generateTypicalIndependentDfr}
\alias{generateTypicalIndependentDfr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate data.frame with independent categorical and numerical columns
}
\description{
Use random functions (like \code{\link{randomCategoricalVector}} and
\code{\link{typicalRandomNorm}}) to generate a random data.frame.
}
\usage{
generateTypicalIndependentDfr(numCat, numCnt, numObs, catProbs=rep(1/3,3),
  rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=0,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numCat}{
How many factor columns to generate
}
  \item{numCnt}{
How many numerical columns to generate
}
  \item{numObs}{
How many observations generate
}
  \item{catProbs}{
see \code{\link{randomCategoricalVector}}. Defaults to 1.
}
  \item{rcnt}{
Function like \code{\link{rnorm}} or \code{\link{typicalRandomNorm}} to randomly
generate continues variables. Only their first parameter will be supplied (i.e.
the number of observations), so you may have to write a custom function.
}
  \item{doShuffle}{
shuffle the order of the columns. Defaults to \code{TRUE}.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Passed on to \code{randomCategoricalVector}
}
}
\value{
data.frame with \code{numObs} rows and \code{numCat+numCnt} columns.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
generateTypicalIndependentDfr(5,5,20,catProbs=randomProbabilities, verbosity=1, minn=2, maxn=4)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~random }
\keyword{ ~data.frame }
