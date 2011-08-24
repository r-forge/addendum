\name{doFormulaCV}
\alias{doFormulaCV}
\alias{crossPredictBinary}
\alias{fitAndPredictBinary.General}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crossvalidate some binary classification
}
\description{
\code{doFormulaCV} is the typical implementation for crossvalidating when a formula
is passed to a fitting function. The other are helpers to implement slightly more
complicated cases.
}
\usage{
doFormulaCV(dfr, outcol, fitFunc, fitAndPredictBinary,
mainname = paste("m", as.character(match.call()$fitFunc), sep = ""),
classname = paste(as.character(match.call()$fitFunc), "ex", sep = ""),
formulaPattern = "X", passMainRes = FALSE, verbosity = 0, ...)
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
What function will do the actual fitting (e.g. \code{gam} from \code{mgcv})
}
  \item{fitAndPredictBinary}{
either a list that may contain items \code{fitFunc}, \code{predictType} and \code{predictMember},
either a character that represents the \code{predictType}, or a function similar to
\code{fitAndPredictBinary.General}. See details.
}
  \item{mainname}{
Name that will be given in the result list to the crossvalidated predicted outcomes.
}
  \item{classname}{
class that will be assgnied to the resulting list
}
  \item{formulaPattern}{
like argument \code{patternNumericCols} of \code{basisExpansionFormula}. Defaults to \code{"X"}.
}
  \item{passMainRes}{
if TRUE, the fitting function is attempted first on the full dataset, and the result
of this fit is passed on in the crossvalidation. Sometimes this can serve as beforehand
estimate of some parameters.
}
  \item{verbosity}{
The higher this value, the more levels of progress and debug information is displayed (note: in R for Windows, turn off buffered output)
}
  \item{\dots}{
Extra parameters passed on to \code{fitFunc}
}
}
\details{
These functions are mainly provided so a generalized approach for comparing binary
classification is possible.

If \code{doFormulaCV} or \code{crossPredictBinary} receive either a list or a character
in the \code{fitAndPredictBinary} parameter, there is no need to implement a custom
function: the work is then relayed to \code{fitAndPredictBinary.general}: the \code{fitFunc}
is applied, and then \code{predict} is called upon the resulting fit, with type=\code{predictType},
and if this is passed along, the \code{predictMember} is used from this result. If this result is
numerical, it is assumed that it represents a probability, so it the class with the greatest
probability (of the two) is used. This is OK for nearly all known binary classification
algorithms in R, but e.g. not for \code{gbm} in package \code{gbm}.

Contrary to \code{doFormulaContCV}, this function does not already do the evaluation. However,
since the return value is so uniform, this can now be easily implemented.
}
\value{
returns a list of class \code{classname}, with first item \code{mainname} the fitting
applied to the full dataset, and second item \code{cv} (crossvalidated) predicted
class for each observation.
}
\author{
Nick Sabbe
}
\note{
To be fully extended in its own package soon
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{doFormulaContCV}}
}
\examples{
#For Gbm, a custom implementation is necessary:
fitAndPredictBinary.Gbm<-function(trainDfr, valDfr, outcol, lvls, formulaPattern=".", verbosity=0, thres=0,...)
{
	require(gbm)
	gf <- basisExpansionFormula(trainDfr, outcol, patternNumericCols=formulaPattern)
	curres=gbm(formula(gf),data=trainDfr,...)
	numToUse<-gbm.perf(curres,method="cv",plot.it=FALSE)
	if(verbosity > 0) cat("numToUse:", numToUse, "\n")
	pred<-predict(curres,newdata=valDfr,type="response",n.trees=numToUse)
	return(lvls[ifelse(pred > thres, 2, 1)])
}
gbmx<-function(thres=0,...){gbm(...)}

doGbm<-function(dfr, outcol, verbosity=0, thres=0,...)#,distribution="adaboost",cv.folds=5,shrinkage=1,bag.fraction=1
{
	require(gbm)
	lvls<-levels(dfr[,outcol])
	dfr[,outcol]<-as.numeric(dfr[,outcol])-1
	rv<-doFormulaCV(dfr, outcol, fitFunc=gbmx, fitAndPredictBinary=fitAndPredictBinary.Gbm,
		verbosity=verbosity, thres=thres, ...)
	rv$cv<-factor(lvls[rv$cv+1], levels=lvls)
	return(rv)
}
iriscopy<-iris
iriscopy$sthing<-factor(rbinom(dim(iris)[1], 1, 0.5))
doGbm(iriscopy, outcol="sthing",
	verbosity=5,distribution="adaboost",cv.folds=10,shrinkage=1,bag.fraction=1,
	verbose=FALSE)

#For bagging, the default can be used:
doBagging<-function(dfr, outcol, verbosity=0,...)#,nbagg=100
{
	require(ipred)
	doFormulaCV(dfr, outcol, fitFunc=bagging, fitAndPredictBinary="class",
		verbosity=verbosity, ...)
}
iriscopy<-iris
iriscopy$sthing<-factor(rbinom(dim(iris)[1], 1, 0.5))
doBagging(iriscopy, outcol="sthing", verbosity=5,nbagg=100)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~binary }
\keyword{ ~classification }
\keyword{ ~crossvalidation }
