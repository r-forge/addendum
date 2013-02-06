\name{plotex}
\alias{plotex}
\alias{simpleplot}
\alias{simpleplot.default}
\alias{simpleplot.glmnet}
\alias{simpleplot.cv.glmnet}
\alias{firstRepeatedAppearance}
\alias{getOrderOfAppearance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots both crossvalidation plot and coefficient plot from glmnet in one plot
}
\description{
Plots both crossvalidation plot and coefficient plot from \code{glmnet} in one graph
}
\usage{
plotex(cvobj, xvar=c("norm", "lambda", "dev"), numTicks=5, lamIndexAxisCol="red", lamIndexAxisPos=NULL, legendPos="topright", legendCex=0.5, legendOf=20, smoothCV=FALSE, errorbarcolor="darkgrey",	centercolor="red", fillsidecolor="#0000ff22", repsNeededForFirstOccurrence=3, beta.type=NULL, ..., verbosity=0, cvup=cvobj$cvup, cvlo=cvobj$cvlo, matplotCols,
altColNameIn=NULL, altColExpressionOut=NULL, showApp=TRUE, skipMoreColors="black", cvby,
addIdxAx=TRUE, addTitle=TRUE)
\method{simpleplot}{glmnet}(object, beta.type=NULL,..., verbosity=0)
\method{simpleplot}{cv.glmnet}(object, beta.type=NULL,..., verbosity=0)
firstRepeatedAppearance(cvobj, repsNeeded)
getOrderOfAppearance(cvobj, repsNeededForFirstOccurrence, showTop=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvobj}{
	result of previous \code{cv.glmnet} call. Note: for This may also simply be a 
	\code{\link{glmnet}} object, in spite of its name (though then some of the other
	parameters lose their meaning).
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
  \item{beta.type}{
see parameter \code{type} of \code{\link{getBeta}}
}
  \item{\dots}{
Extra parameters passed on to \code{plot.glmnet}
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{cvup}{
upper value(s) for the right side axis
}
  \item{cvlo}{
lower value(s) for the right side axis
}
  \item{matplotCols}{
if not missing, these colors (and in this order) are used for the \code{glmnet} plot and
the legend. If not, the return value of \code{\link{neatColorSet}} is used.
}
  \item{altColNameIn, altColExpressionOut}{
see \code{\link{colorsForPlotEx}}
}
  \item{showApp}{
see \code{\link{addLegendFromColData}}
}
  \item{skipMoreColors}{
if passed along, these colors are skipped from the colors of \code{matplotCols}. Default is "black".
}
  \item{cvby}{
passed as \code{by} argument to \code{\link{addCVPlot}}.
}
  \item{addIdxAx, addTitle}{
see \code{\link{addLamIndexAxis}}
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
	\item{showTop}{
if not \code{NULL} (the default), only the first \code{showTop} occurring variables are returned
}
}
\details{
\code{simpleplot} is really just a helper to allow extension to \code{\link{glmnet}}-
like objects.
}
\value{
The result of the \code{\link{addCVPlot}} call is returned invisibly.

\code{getOrderOfAppearance} returns a list with items \code{legendForVars} (variable names ordered
by occurrence), \code{whereAppearing} (at which lambda index does this variable occur for the first 
time), \code{whereAsInPlotCoef} (which variable indices as used in \code{\link{plotCoef}} --- note
the variables that are always zero do not occur there), \code{orderOfFirstAppearance} (which 
variable indices in the set of all indices)
}
\author{
Nick Sabbe
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{addCVPlot}}
}
\examples{
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
\keyword{ glmnet }
\keyword{ plot }
\keyword{ crossvalidation }
