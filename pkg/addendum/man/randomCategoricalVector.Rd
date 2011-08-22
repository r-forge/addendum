\name{randomCategoricalVector}
\alias{randomCategoricalVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create vector wih random number of classes and probabilities
}
\description{
Use given class probabilities or function to generate these, to create a random vector.
}
\usage{
randomCategoricalVector(numObs, catProbs=rep(1/3,3), asFactor=FALSE, verbosity=0,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numObs}{
Number of items in the random vector
}
  \item{catProbs}{
Either a (optionally named) vector containing class probabilities, or a function
(like \code{\link{randomProbabilities}}) that can randomly generate them.
}
  \item{asFactor}{
if \code{TRUE}, the result is returned as a factor instead of a character vector. Defaults to \code{FALSE}.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Passed on to \code{catProbs} (if it is a function).
}
}
\value{
character vector/factor of length \code{numObs}. Its \code{probs} attribute holds the probabilities per "class".
}
\author{
Nick Sabbe
}
\details{
If names for the classes are not provided (e.g. if \code{catProbs} is an unnamed
vector or a function that returns an unnamed vector), the first letters of the
alphabet are used (note: this may fail if more than 26 classes are used...).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{randomProbabilities}}
}
\examples{
randomCategoricalVector(20, catProbs=rep(1/3,3), verbosity=1)
randomCategoricalVector(20, catProbs=randomProbabilities, verbosity=1, minn=3)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~random }
\keyword{ ~categorical }
