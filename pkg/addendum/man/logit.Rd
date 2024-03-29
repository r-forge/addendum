\name{logit}
\alias{logit}
\alias{expit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
logit and expit transformation on data
}
\description{
Perform the probit transformation on a probability, or the expit transformation
on a real number (or a vector of them)
}
\usage{
logit(p, adjust)
expit(x, underlim=-700, upperlim=16.81)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
probabilities to transform
}
  \item{adjust}{
Adjustment for probabilities that are 0 or 1. If it is needed but not provided,
defaults to 0.025.
}
  \item{x}{
real number(s) to perform the expit transform on.
}
  \item{underlim}{
due to numerical constraints, calculation below this value result in probability 0 or even \code{NaN}. On my own workstation, this is \code{-700}, the default.
}
  \item{upperlim}{
due to numerical constraints, calculation above this value result in probability 1 or even \code{NaN}. On my own workstation, this is \code{16.81}, the default.
}
}
\value{
numeric vector of the same length as the original.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
logit(c(0,0.3,0.7,1))
logit(c(0,0.3,0.7,1), adjust=0)
expit(seq(-5, 5, length.out=10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ probit }
\keyword{ exit }
