\name{qrmvnorm}
\alias{qrmvnorm}
\alias{qrmvnormsqrt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sped up version of rmvnorm
}
\description{
\code{qrmvnorm}: sped up version of rmvnorm, leaving out all sanity checks and always
using choleski.
\code{qrmvnormsqrt}: sped up version of rmvnorm, using a choleski decomposition of the covar matrix.
in quite a few cases we already know the chol decomp of sigma!
}
\usage{
qrmvnorm(n, mean, sigma)
qrmvnormsqrt(n, mean, cholsigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
number of observations to generate
}
  \item{mean}{
means for each covariate
}
  \item{sigma}{
covariance matrix
}
  \item{cholsigma}{
choleski decomposition of the covariance matrix
}
}
\value{
matrix holding \code{n} rows and a column for each covariate requested.
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[mvtnorm]{rmvnorm}}
}
\examples{
x<-round(upper.tri(matrix(1, 10, 10), diag = TRUE))
sgm<-x \%*\% t(x)
qrmvnorm(10, rep(0,10), sgm)
csgm<-chol(sgm, pivot=TRUE)
qrmvnormsqrt(10, rep(0,10), csgm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~rmvnorm }
\keyword{ ~random }
\keyword{ ~multivariate }
\keyword{ ~normal }
