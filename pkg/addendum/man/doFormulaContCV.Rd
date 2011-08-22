\name{doFormulaContCV}
\alias{doFormulaContCV}
\alias{crossPredictContinuous}
\alias{fitAndPredictContinuous.General}
\alias{createCVResult}
\alias{evaluatePredictions.lms}
\alias{getFormulaCVDescription}
\alias{print.CVRes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crossvalidate some continuous prediction fit
}
\description{
\code{doFormulaContCV} is the typical implementation for crossvalidating when a formula
is passed to a fitting function. The other are helpers to implement slightly more
complicated cases.
}
\usage{
doFormulaContCV(dfr, outcol, fitFunc, fitAndPredictContinuous, formulaPattern = "X",
evaluatePredictions = evaluatePredictions.lms, includePreds = FALSE,
includeDesc = FALSE, verbosity = 0, ffDesc, ...)
\method{print}{CVRes}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
Dataframe on which to crossvalidate
}
  \item{outcol}{
Column name of the outcome
}
  \item{fitFunc}{
What function will do the actual fitting (e.g. \code{lm})
}
  \item{fitAndPredictContinuous}{
either a list that may contain items \code{fitFunc}, \code{predictType} and \code{predictMember},
either a character that represents the \code{predictType}, or a function similar to
\code{fitAndPredictContinuous.General}. See details.
}
  \item{formulaPattern}{
like argument \code{patternNumericCols} of \code{basisExpansionFormula}. Defaults to \code{"X"}.
}
  \item{evaluatePredictions}{
function that is used to evaluate the crossvalidated results. Defaults to
\code{evaluatePredictions.lms} which does a least square error.
}
  \item{includePreds}{
if true, the actual predicted values are included in the return value
}
  \item{includeDesc}{
if true, a description for this fit is built from the parameters (name of the \code{fitFunc}
and others) and included in the result.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
  \item{ffDesc}{
In some cases, it is better to pass the name of the \code{fitFunc} directly because
R will not be able to retrieve it (depends on call depth etc.)
}
  \item{\dots}{
Extra parameters passed on to the fit function.
}
  \item{x}{
Object to be printed
}
}
\details{
These functions are mainly provided so a generalized approach for comparing binary
classification is possible.

If \code{doFormulaContCV} or \code{crossPredictContinuous} receive either a list or a character
in the \code{fitAndPredictContinuous} parameter, there is no need to implement a custom
function: the work is then relayed to \code{fitAndPredictContinuous.General}: the \code{fitFunc}
is applied, and then \code{predict} is called upon the resulting fit, with type=\code{predictType},
and if this is passed along, the \code{predictMember} is used from this result. If this result is
numerical, it is assumed that it represents a probability, so it the class with the greatest
probability (of the two) is used. This is OK for nearly all known continuous prediction
algorithms in R, but e.g. not for \code{gbm} in package \code{gbm}.
}
\value{
If \code{includePreds} and \code{includeDesc} are both false, the result is simply
the result of \code{evaluatePredictions}.

If not, this result is the first item (\code{evaluated}) of a list, that may also
contain \code{preds} (the actual (crossvalidated) predicted values for each observation)
and \code{desc} (see the code of \code{getFormulaCVDescription} for how this is built up).

If \code{includeDesc} was TRUE, the class of this result is \code{CVRes}.

A custom \code{print} function is provided for objects of class \code{CVRes}.
}
\author{
Nick Sabbe
}
\note{
To be fully extended in its own package soon
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{doFormulaCV}}
}
\examples{
#For Gbm, a custom implementation is necessary:
fitAndPredictContinuous.GbmCont<-function(trainDfr, valDfr, outcol, formulaPattern=".", verbosity=0,...)
{
	gf <- basisExpansionFormula(trainDfr, outcol, patternNumericCols=formulaPattern)
	curres=gbm(formula(gf),data=trainDfr,...)
	numToUse<-gbm.perf(curres,method="cv",plot.it=FALSE)
	if(verbosity > 0) cat("numToUse:", numToUse, "\n")
	pred<-predict(curres,newdata=valDfr,type="response",n.trees=numToUse)
	return(pred)
}

doGbmCont<-function(dfr, outcol, verbosity=0,...)#,distribution="adaboost",cv.folds=5,shrinkage=1,bag.fraction=1
{
	require(gbm)
	lvls<-levels(dfr[,outcol])
	dfr[,outcol]<-as.numeric(dfr[,outcol])-1
	rv<-doFormulaContCV(dfr, outcol, fitAndPredictContinuous=fitAndPredictContinuous.GbmCont,
		verbosity=verbosity, ...)
	#rv$cv<-factor(lvls[rv$cv+1], levels=lvls)
	return(rv)
}
doGbmCont(iris, outcol="Sepal.Length", verbosity=5,distribution="gaussian",
  cv.folds=10,bag.fraction=1,shrinkage=1,n.trees=200, verbose=FALSE)
#For Random forest, the default works:
doRandomForestCont<-function(dfr, outcol, verbosity=0,...)#,ntree=500
{
	require(randomForest)
	doFormulaContCV(dfr, outcol, fitFunc=randomForest, fitAndPredictContinuous="",
		verbosity=verbosity, ...)
}
doRandomForestCont(iris, outcol="Sepal.Length",
	verbosity=5,ntree=500)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~continuous }
\keyword{ ~prediction }
\keyword{ ~crossvalidation }
