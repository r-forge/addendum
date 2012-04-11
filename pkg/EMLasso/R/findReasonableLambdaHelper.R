#' Function to run on a dataset with not too much missing data to identify a set of reasonable lambda values
#' 
#' Expects a singly imputed dataset and fits a logistic LASSO so the user
#' can pick a set that wil probably be interesting.
#' 
#' @param ds dataset to investigate
#' @param out outcome vector
#' @param family see \code{\link{glmnet}}. Defaults to "binomial" (i.e. lasso penalized logistic regression).
#' @param showFirst show the top coefficients (first \code{showFirst} occurring)
#' @param showPlot if \code{TRUE} (the default), visually supports the decision
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param repsNeededForFirstOccurrence How many times (i.e. for how many lambda values)
#' must a coefficient be consecutively nonzero before we count it as "occurring"
#' @param \dots passed on to \code{\link{plotex}} (if relevant)
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param minNumHigher How many lambdas higher than the optimum do you minimally want (if available)
#' @param minNumLower How many lambdas lower than the optimum do you minimally want (if available)
#' @param maxNumLower How many lambdas lower than the optimum do you maximally want
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}
#' @param standardize see \code{\link{glmnet}}. Defaults to FALSE.
#' @return list of class "LambdaHelper":
#' \item{topres }{\code{data.frame} with \code{showFirst} rows, and columns: 
#' \code{variable} (name), \code{lambda},\code{critl} (lower bound of criterion),
#' \code{crit} (estimate of criterion), \code{critu} (upper bound of criterion), 
#' \code{critsd} (sd of criterion), \code{index} (at which lambda index does this variable first occur)} 
#' \item{allLambda }{vector of lambda values}
#' \item{regionDfr }{\code{data.frame} w 3 rows 3 columns: \code{name} (values: "lower lambda", "optimum", and "higher lambda"), \code{idx} and \code{lambda}}
#' \item{regionOfInterestData }{see \code{\link{getMinMaxPosLikeGlmnet}}}
#' @note EMLasso is pretty heavy and has to be run per lambda. This functions helps
#' preselect some lambda values, and can typically avoid useless calculations for
#' non-interesting lambda values.
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords glmnet lambda
#' @examples aDfr<-generateTypicalIndependentDfr(numCat=10, numCnt=10, numObs=100, catProbs=rep(1/3,3),
#' rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=1)
#' 
#' outlins<- -mean(aDfr$cnt1)+aDfr$cnt1+2*(aDfr$cat1=="b")
#' outprobs<-expit(outlins)
#' y<-factor(sapply(outprobs, function(prob){sample(c("no", "yes"), 1, prob=c(1-prob,prob))}))
#' 
#' rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
#' @export
findReasonableLambdaHelper<-function(ds, out, family="binomial", showFirst=20, showPlot=TRUE,
	type.measure="auc", repsNeededForFirstOccurrence=3, ..., verbosity=0,
	minNumHigher=20, minNumLower=20, maxNumLower=30, 
	imputeDs2FitDsProperties=normalImputationConversion(), standardize=FALSE)
{
	cv<-fit.glmnet(ds=ds, out=out, lambda=NULL, verbosity=verbosity-1, 
		standardize=standardize, type.measure=type.measure, 
		imputeDs2FitDsProperties=imputeDs2FitDsProperties, family=family)
	fit<-cv$glmnet.fit
	if(showPlot)
	{
		plotex(cv, xvar="lambda", numTicks=10, label=TRUE,
			legendOf=showFirst,..., verbosity=verbosity-1)
	}
	#kolommen van beta zijn lambdas, van groot (s0) naar klein (s99)
	firstAppearance<-firstRepeatedAppearance(cv, repsNeededForFirstOccurrence)
	#firstAppearance now holds, for every variable that ever appears, the index
	#within the lambdas where it first appears.
	orderOfFirstAppearance<-order(firstAppearance)[1:showFirst]
	whereAppearing<-firstAppearance[orderOfFirstAppearance]
	forVars<-names(firstAppearance)[orderOfFirstAppearance]

	regionOfInterestData<-getMinMaxPosLikeGlmnet(cv)
	ll<-length(regionOfInterestData$lambda)
	sgn<-sign(regionOfInterestData$lambda[1] - regionOfInterestData$lambda[ll])
	#if sgn is positive (+1), lambda[1] is bigger than the last one, so ordered from big to small
	if(abs(regionOfInterestData$pos.optimum - regionOfInterestData$pos.higherlambda) < minNumHigher)
	{
		regionOfInterestData$pos.higherlambda<-regionOfInterestData$pos.optimum-(sgn*minNumHigher)
		if(regionOfInterestData$pos.higherlambda<1)
		{
			regionOfInterestData$pos.higherlambda<-1
		}
		if(regionOfInterestData$pos.higherlambda>ll)
		{
			regionOfInterestData$pos.higherlambda<-ll
		}
	}
	actualNumHigher<-abs(regionOfInterestData$pos.higherlambda-regionOfInterestData$pos.optimum)
	catwif(verbosity > 0, "actualNumHigher: ", actualNumHigher, ", as opposed to minNumHigher:", minNumHigher)
	if(actualNumHigher < minNumHigher)
	{
		#add this to the 'lower' side:
		minNumLower <- minNumLower + (minNumHigher - actualNumHigher)
		maxNumLower <- maxNumLower + (minNumHigher - actualNumHigher)
	}
	diff1<-abs(regionOfInterestData$pos.lowerlambda1 - regionOfInterestData$pos.optimum)
	diff2<-abs(regionOfInterestData$pos.lowerlambda2 - regionOfInterestData$pos.optimum)
	if(diff1 < minNumLower) diff1<-minNumLower
	if(diff2 < minNumLower) diff2<-minNumLower
	if(diff1 > maxNumLower) diff1<-maxNumLower
	if(diff2 > maxNumLower) diff2<-maxNumLower
	diff<-min(diff1, diff2)
	regionOfInterestData$pos.lowerlambda<-regionOfInterestData$pos.optimum + (sgn*diff)
	if(regionOfInterestData$pos.lowerlambda<1)
	{
		regionOfInterestData$pos.lowerlambda<-1
	}
	if(regionOfInterestData$pos.lowerlambda>ll)
	{
		regionOfInterestData$pos.lowerlambda<-ll
	}
	poss<-c(regionOfInterestData$pos.lowerlambda, regionOfInterestData$pos.optimum, regionOfInterestData$pos.higherlambda)
	tmp<-regionOfInterestData$lambda[poss]
	regionDfr<-data.frame(name=c("lower lambda", "optimum", "higher lambda"), idx=poss, lambda=tmp, stringsAsFactors=FALSE)
	if(verbosity > 0)
	{
		catw("Lambdas used:")
		print(regionDfr)
	}
	
	retval<-list(topres=data.frame(variable=forVars, lambda=fit$lambda[whereAppearing],
		critl=cv$cvlo[whereAppearing], crit=cv$cvm[whereAppearing],
		critu=cv$cvup[whereAppearing], critsd=cv$cvsd[whereAppearing],
		index=whereAppearing, stringsAsFactors=FALSE), allLambda=fit$lambda,
		regionDfr=regionDfr, regionOfInterestData=regionOfInterestData)
	class(retval)<-"LambdaHelper"
	return(retval)
}

#' @rdname findReasonableLambdaHelper
#' 
#' @usage \method{[}{LambdaHelper}(object, i, j, drop = TRUE)
#' @param object \code{LambdaHelper}
#' @param i row index
#' @param j column index. If this is missing, the \code{i}th lambda is returned
#' @param drop if \code{TRUE} the result is coerced to the simplest structure possible
#' @return depends on the parameters
#' @export
#' @aliases [.LambdaHelper
#' @method [ LambdaHelper
#' @examples data(emlcvfit, package="EMLasso")
#' rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
#' rlh[1]
#' rlh[1:5, NULL]
`[.LambdaHelper`<-function(object, i, j, drop = TRUE)
{
	if(missing(j)) return(object$topres$lambda[i])
	if(is.null(j) || is.na(j) || (j=="")) return(object$topres[i, seq(ncol(object$topres)), drop=drop])
	else return(object$topres[i, j, drop=drop])
}

#' @rdname findReasonableLambdaHelper
#' 
#' @param x object to find 'interesting' set of lambdas for
#' @return vector of lambda values, normally high to low
#' @keywords lambda
#' @export
getLambdas<-function(x,...) UseMethod("getLambdas")


#' @rdname findReasonableLambdaHelper
#' 
#' @inheritParams getLambdas
#' @method getLambdas lambdaregion
#' @usage \method{getLambdas}{lambdaregion}(x, ...)
#' @export
#' @examples data(emlcvfit, package="EMLasso")
#' rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
#' getLambdas(rlh$regionOfInterestData)
getLambdas.lambdaregion<-function(x,...)
{
	x$lambda[(x$pos.higherlambda):(x$pos.lowerlambda)]
}

#' @rdname findReasonableLambdaHelper
#' 
#' @inheritParams getLambdas
#' @method getLambdas LambdaHelper
#' @usage \method{getLambdas}{LambdaHelper}(x, ...)
#' @export
#' @examples data(emlcvfit, package="EMLasso")
#' rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
#' getLambdas(rlh)
getLambdas.LambdaHelper<-function(x,...)
{
	getLambdas(x$regionOfInterestData, ...)
}