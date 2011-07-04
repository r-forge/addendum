\name{plot2.cv.glmnet}
\alias{plot2.cv.glmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots both crossvalidation plot and coefficient plot from glmnet in one plot
}
\description{
Plots both crossvalidation plot and coefficient plot from \code{glmnet} in one graph
}
\usage{
plot2.cv.glmnet(cvobj, xvar = c("norm", "lambda", "dev"), numTicks = 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvobj}{
result of previous \code{cv.glmnet} call
}
  \item{xvar}{
What is on the X-axis. \code{"norm"} plots against the L1-norm of the coefficients,
\code{"lambda"} against the log-lambda sequence, and \code{"dev"} against the percent
deviance explained. See \code{plot.glmnet}
}
  \item{numTicks}{
How many ticks should be presented on the righthand axis for the crossvalidation value.
}
  \item{\dots}{
Extra parameters passed on to \code{plot.glmnet}
}
}
\value{
Nothing is returned
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{addCVPlot}}
}
\examples{
require(glmnet)
set.seed(1010)
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= (x[,seq(nzc)] \%*\% beta)
eps=rnorm(n)*5
y=drop(fx+eps)
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)
cvob1=cv.glmnet(x,y)
plot2.cv.glmnet(cvob1, xvar="lambda", numTicks=5)
title("Gaussian Family",line=2.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~glmnet }
\keyword{ ~plot }
\keyword{ ~crossvalidation }
