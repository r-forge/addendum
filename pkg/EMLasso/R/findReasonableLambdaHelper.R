#' Function to run on a dataset with not too much missing data to identify a set of reasonable lambda values
#' 
#' Expects a singly imputed dataset and fits a logistic LASSO so the user
#' can pick a set that wil probably be interesting.
#' 
#' @param ds dataset to investigate
#' @param out outcome vector
#' @param showFirst show the top coefficients (first \code{showFirst} occurring)
#' @param showPlot if \code{TRUE} (the default), visually supports the decision
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param repsNeededForFirstOccurrence How many times (i.e. for how many lambda values)
#' must a coefficient be consecutively nonzero before we count it as "occurring"
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return list of class "LambdaHelper":
#' \item{topres }{\code{data.frame} with \code{showFirst} rows, and columns: 
#' \code{variable} (name), \code{lambda},\code{critl} (lower bound of criterion),
#' \code{crit} (estimate of criterion), \code{critu} (upper bound of criterion), 
#' \code{critsd} (sd of criterion), \code{index} (at which lambda index does this variable first occur)} 
#' \item{allLambda }{vector of lambda values}
#' @note EMLasso is pretty heavy and has to be run per lambda. This functions helps
#' preselect some lambda values, and can typically avoid useless calculations for
#' non-interesting lambda values.
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords glmnet lambda
#' @examples data(emlcvfit, package="EMLasso")
#' rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
#' @export
findReasonableLambdaHelper<-function(ds, out, showFirst=20, showPlot=TRUE,
	type.measure="auc", repsNeededForFirstOccurrence=3, verbosity=0)
{
	cv<-fit.lognet(dfr=ds, resp=out, lambda=NULL, verbosity=verbosity-1,
		type.measure=type.measure)
	fit<-cv$glmnet.fit
	if(showPlot)
	{
		plotex(cv, xvar="lambda", numTicks=10, label=TRUE,
			legendOf=showFirst, verbosity=verbosity-1)
	}
	#kolommen van beta zijn lambdas, van groot (s0) naar klein (s99)
	firstAppearance<-firstRepeatedAppearance(cv, repsNeededForFirstOccurrence)
	#firstAppearance now holds, for every variable that ever appears, the index
	#within the lambdas where it first appears.
	orderOfFirstAppearance<-order(firstAppearance)[1:showFirst]
	whereAppearing<-firstAppearance[orderOfFirstAppearance]
	forVars<-names(firstAppearance)[orderOfFirstAppearance]

	retval<-list(topres=data.frame(variable=forVars, lambda=fit$lambda[whereAppearing],
		critl=cv$cvlo[whereAppearing], crit=cv$cvm[whereAppearing],
		critu=cv$cvup[whereAppearing], critsd=cv$cvsd[whereAppearing],
		index=whereAppearing, stringsAsFactors=FALSE), allLambda=fit$lambda)
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