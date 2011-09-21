\name{addCVPlot}
\alias{addCVPlot}
\alias{getXIndices}
\alias{gn.error.bars}
\alias{addLamIndexAxis}
\alias{getBeta}
\alias{getBeta.cv.glmnet}
\alias{getBeta}
\alias{getBeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add crossvalidation plot to standard glmnet plot
}
\description{
\code{glmnet} provides a way to plot the values of coefficients for different values of
optimization parameter \code{lambda}. It also provides a way to plot crossvalidation
results. This method adds the crossvalidation plot on top of the coefficient plot.
}
\usage{
addCVPlot(cvobj, xvar=c("norm", "lambda", "dev"), numTicks, smoothed=FALSE, errorbarcolor="darkgrey", centercolor="red", fillsidecolor="#0000ff22", verbosity=0)
getXIndices(cvobj, xvar=c("norm", "lambda", "dev"), verbosity=0)
\method{getBeta}{cv.glmnet}(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvobj}{
result of \code{cv.glmnet}
}
  \item{xvar}{
What is on the X-axis. \code{"norm"} plots against the L1-norm of the coefficients,
\code{"lambda"} against the log-lambda sequence, and \code{"dev"} against the percent
deviance explained. Should be identical as the one passed to \code{plot.glmnet}
}
  \item{numTicks}{
How many ticks should be presented on the righthand axis for the crossvalidation value.
}
  \item{smoothed}{
If \code{TRUE}, the error flags and CV values are not shown as error flags, but
in a smoothed form.
}
  \item{errorbarcolor}{
Color in which the error flags or upper limit (\code{smoothed}) are drawn
}
  \item{centercolor}{
Color in which the CV values are drawn
}
  \item{fillsidecolor}{
When \code{smoothed=TRUE} the region between the top and bottom value is filled
with this color. Advised to ensure that it is transparent (low alpha value)
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{object}{
object for which the beta must be obtained
}
}
\details{
Available for customization, but in general probably easier used as part of \code{\link{plotex}}

\code{getXIndices} is a helper function that recreates some code present in \code{plot.cv.glmnet}, and so is \code{gn.error.bars}.

\code{addLamIndexAxis} can be used to add an extra axis (at the bottom) showing which of the (typically) 100 \code{lambda} values is shown.
}
\value{
Invisibly, a \code{\link{data.frame}} holding the smoothed/transformed plot values is returned.
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotex}}
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
print(class(cvob1))
plot(cvob1$glmnet.fit, xvar="lambda")
addCVPlot(cvob1, xvar = "lambda", numTicks=5)
title("Gaussian Family",line=2.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~glmnet }
\keyword{ ~plot }
\keyword{ ~crossvalidation }
