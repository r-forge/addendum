\name{plotROCFromRepPredProb}
\alias{plotROCFromRepPredProb}
\alias{AUCFromRepPredProb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the ROC from a matrix of predicted probabilities
}
\description{
Plot the ROC from a matrix of predicted probabilities for several observations (rows)
and repeats (cols), given the true outcome for the observations.
}
\usage{
plotROCFromRepPredProb(obsrepprob, out, thres=seq(0,1, length.out=round(min(dim(obsrepprob))/2, 1)), doPlot=TRUE, showEquiDistThres=10, verbosity=0)
AUCFromRepPredProb(ROCFromRepPredProb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obsrepprob}{
matrix with probabilities per observation (rows) and per repeat (columns)
}
  \item{out}{
true outcome for the observations. Best if this is either logical (true being "cases") or a binary factor
}
  \item{thres}{
Possible thresholds that are investigated. By default there are half as many of these as the minimum 
dimension of \code{obsrepprob}, regularly spaced between 0 and 1. This is always corrected so it 
contains 0 and 1 and is sorted.
}
  \item{doPlot}{
if \code{TRUE} (default), a plot is produced, otherwise only the result is returned.
}
  \item{showEquiDistThres}{
show at some points the value of the threshold. This number (default 10) indicates at how many equidistant
(within the used/given thresholds) points this happens.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is
displayed (note: in R for Windows, turn off buffered output)
}
  \item{ROCFromRepPredProb}{
matrix or other object of the same form as the return value of \code{plotROCFromRepPredProb}
}
}
\details{
\code{AUCFromRepPredProb} is a convenience function that calculates the AUC from the kind of matrix
like \code{plotROCFromRepPredProb} produces.
}
\value{
a matrix with three columns: "threshold", holding all used values of the threshold, "TPR"
holding the matching value for TPR and "FPR" similar for FPR.
}
\author{
Nick Sabbe (nick.sabbe@ugent.be)
}
\examples{
y<-factor(rbinom(100, 1, 0.5), labels=c("no", "yes"))
#Normally, these would come from some repeated model fitting (e.g. MI)
probs<-matrix(NA, nrow=length(y), ncol=30)
probs[y=="yes",]<-1-rbeta(sum(y=="yes")*30, 2, 5)
probs[y=="no",]<-rbeta(sum(y=="no")*30, 2, 5)

res<-plotROCFromRepPredProb(obsrepprob=probs, out=y, doPlot=FALSE, verbosity=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ ROC }
