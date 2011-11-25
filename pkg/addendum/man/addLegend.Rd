\name{addLegend}
\alias{addLegend}
\alias{addLegendFromColData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add legend to standard (cv.)glmnet plot
}
\description{
Add legend to standard (cv.)glmnet plot, to show which variables occur at which lambda.
}
\usage{
addLegend(cvobj, matplotCols=1:6, repsNeededForFirstOccurrence=3, topHowMany=20, beta.type=NULL,
	excludergb=NULL, legendPos="topright", legendCex=0.5)
addLegendFromColData(coldata, legendPos="topright", legendCex=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvobj}{
result of \code{\link{cv.glmnet} or \code{\link{glmnet}
}
  \item{matplotCols}{
colors that have been passed to \code{\link{plot.glmnet}}. IF \code{NULL}, \code{\link{neatColorSet}}
is used with parameter \code{excludergb}.
}
  \item{repsNeededForFirstOccurrence}{
See \code{\link{colorsForPlotEx}}.
}
  \item{topHowMany}{
See \code{\link{colorsForPlotEx}}.
}
  \item{beta.type}{
See \code{\link{colorsForPlotEx}}.
}
  \item{excludergb}{
if \code{matplotCols} was \code{NULL}, this is passed to \code{\link{neatColorSet}}
}
  \item{legendPos}{
Where does the legend occur. Notice this has to be one of the text versions that can be passed
to \code{\link{legend}}
}
  \item{legendCex}{
code{cex} that is passed to \code{legend}
}
  \item{coldata}{
result of a call to \code{\link{colorsForPlotEx}}
}
}
\value{
Nothing
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{colorsForPlotEx}}, \code{\link{neatColorSet}}, \code{\link{plot.glmnet}}, \code{\link{legend}}
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
plot(cvob1)
addLegend(cvobj=cvob1, matplotCols=NULL, topHowMany=10, legendPos="topleft")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glmnet }
\keyword{ plot }
\keyword{ crossvalidation }
\keyword{ legend }
