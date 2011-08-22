\name{randomRandomNorm}
\alias{randomRandomNorm}
\alias{typicalRandomNorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample from normal distribution with random mean and sd.
}
\description{
Sample from a normal distribution where the mean and variance are uniformly
sampled between given boundaries. \code{typicalRandomNorm} is simply a convenience
wrapper where the boundaries are obtained from 1 parameter.
}
\usage{
randomRandomNorm(n, minmu=0, maxmu=minmu, minsig2=1, maxsig2=minsig2)
typicalRandomNorm(n, absmu=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
How many items to sample from this distribution
}
  \item{minmu}{
Minimum value for the mean. Defaults to 0.
}
  \item{maxmu}{
Maximum value for the mean. Defaults to \code{minmu}.
}
  \item{minsig2}{
Minimum value for the variance. Defaults to 1.
}
  \item{maxsig2}{
Maximum value for the variance. Defaults to \code{minsig2}.
}
  \item{absmu}{
Outer limit (negative and positive) for mean and maximum for variance (the minimum is 0). Defaults to 10.
}
}
\value{
vector of length \code{n}. Its \code{mu} and \code{sig2} attributes hold the used mean and variance.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
randomRandomNorm(20)
randomRandomNorm(20, minmu=-3, maxmu=3, minsig2=0, maxsig2=2)
randomRandomNorm(20, minmu=-3, maxmu=3)
typicalRandomNorm(20)
typicalRandomNorm(20, 0.5)
typicalRandomNorm(20, 2)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~normal }
\keyword{ ~random }
