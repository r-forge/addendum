\name{colorsForPlotEx}
\alias{colorsForPlotEx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Colors information that works similar to plotex and plot.glmnet
}
\description{
Returns how colors are used in \code{\link{plotex}} (and \code{\link{plot.glmnet}}). This is useful e.g. 
for displaying a legend (as in \code{\link{addLegend}})
}
\usage{
colorsForPlotEx(cvobj, matplotCols, repsNeededForFirstOccurrence=3, topHowMany=20, beta.type=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvobj}{
\code{glmnet} or \code{cv.glmnet} object
}
  \item{matplotCols}{
Colors that would be passed on to \code{\link{matplot}} by \code{\link{plot.glmnet}}
}
  \item{repsNeededForFirstOccurrence}{
See \code{\link{getOrderOfAppearance}}
}
  \item{topHowMany}{
See \code{showTop} in \code{\link{getOrderOfAppearance}}
}
  \item{beta.type}{
See \code{\link{getOrderOfAppearance}}
}
}
\value{
A list with the following items:
\item{matplotCols}{passed in \code{matplotCols}, potentially enlarged to match the number of variables printed}
\item{legendForVars}{names of first \code{} variables that would be plotted by \code{\link{plot.glmnet}}}
\item{whereAppearing}{at which index of the lambdas does each of the top variables occur "for the first time"" (see \code{\link{getOrderOfAppearance}})}
\item{useColors}{the colors that the top variables are plotted in}
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

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
colorsForPlotEx(cvob1, matplotCols=neatColorSet(excludergb=col2rgb("black")), topHowMany=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
\keyword{ top }
\keyword{ variable }
\keyword{ glmnet }
