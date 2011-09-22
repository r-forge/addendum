\name{plotex}
\alias{plotex}
\alias{simpleplot}
\alias{simpleplot.default}
\alias{simpleplot.cv.glmnet}
\alias{firstRepeatedAppearance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots both crossvalidation plot and coefficient plot from glmnet in one plot
}
\description{
Plots both crossvalidation plot and coefficient plot from \code{glmnet} in one graph
}
\usage{
plotex(cvobj, xvar=c("norm", "lambda", "dev"), numTicks=5, lamIndexAxisCol="red", lamIndexAxisPos=NULL, legendPos="topright", legendCex=0.5, legendOf=20, smoothCV=FALSE, errorbarcolor="darkgrey",	centercolor="red", fillsidecolor="#0000ff22", repsNeededForFirstOccurrence=3, ..., verbosity=0)
\method{simpleplot}{cv.glmnet}(object,..., verbosity=0)
firstRepeatedAppearance(cvobj, repsNeeded)
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
  \item{lamIndexAxisCol}{
if non-\code{NULL}, an extra axis is added to the bottom in this color: see \code{\link{addLamIndexAxis}} for its meaning
}
  \item{lamIndexAxisPos}{
If \code{lamIndexAxisCol != NULL}, the extra axis is added at this y-position.
If \code{NULL} (the default), the lowest value on the y-axis is used.
}
  \item{legendPos}{
Position in text format (see \code{\link{legend}}) where the legend will appear.
If \code{NULL}, no legend appears. Default is "topright".
}
  \item{legendCex}{
If \code{legendPos != NULL}, the size of the legend text etc is multiplied by
this factor wrt the normal size. Default is \code{0.5}
}
  \item{legendOf}{
If \code{legendPos != NULL}, this number says for how many (in order of
appearance) variables the legend must be presented. Default is \code{20}.
}
  \item{smoothCV}{
If \code{TRUE}, the error flags and CV values are not shown as error flags, but
in a smoothed form.
}
  \item{errorbarcolor, centercolor, fillsidecolor}{
see \code{\link{addCVPlot}}
}
  \item{repsNeededForFirstOccurrence}{
for finding the first occurrence: how many times does the coefficient of a
variable have to be repeatedly nonzero before we call it the actual first
occurrence?
}
  \item{\dots}{
Extra parameters passed on to \code{plot.glmnet}
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{object}{
object for which 'simple' plot must be made (typically: the non-crossvalidation
plot for the object)
}
  \item{repsNeeded}{
for finding the first occurrence: how many times does the coefficient of a
variable have to be repeatedly nonzero before we call it the actual first
occurrence?
}
}
\details{
\code{simpleplot} is really just a helper to allow extension to \code{\link{glmnet}}-
like objects.
}
\value{
The result of the \code{\link{addCVPlot}} call is returned invisibly.
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
plotex(cvob1, xvar="lambda", numTicks=5)
title("Gaussian Family",line=2.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~glmnet }
\keyword{ ~plot }
\keyword{ ~crossvalidation }
