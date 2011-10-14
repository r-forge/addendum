#' Function to calculate predicted probability of a lognet fit
#' 
#' Method that provides information that can be reused to predict
#' probabilities from the same lognet several times
#' 
#' @param lognet binomial \code{\link{glmnet}} fit to use for prediction
#' @param dfr \code{\link{numdfr}} or \code{\link{data.frame}} that holds the structure
#' from which dataset predictions will need to happen; better yet, object of class 
#' "dfrConversionProbs" (see \code{\link{dfrConversionProbs}})
#' @param betweenColAndLevel separator in dummy name coding
#' @param usecol which of the column of beta must be used. Must be provided if \code{lognet}
#' is not the result of a 1-lambda \code{\link{glmnet}} call.
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return object of class "lognetProbWrapper": list with items:
#' \item{conversionProps }{\code{data.frame} with columns \code{fromCols} (original 
#' 	 column number), \code{newNames} (new column name), \code{mustEqual} (which value
#' 	 must it equal for this level of the factor), \code{useBeta} (coefficient)} 
#' \item{originalLognet }{\code{lognet} that was passed in} 
#' \item{usedcol }{usecol that was passed in (or 1 if only it was missing)}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{dfrConversionProbs}}
#' @keywords lognet glmnet predict dummy
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' irisdummy<-factorsToDummyVariables(iris, verbosity=10)
#' lnet<-glmnet(irisdummy, y, family="binomial")
#' lognetProbWrapper(lnet, iris, betweenColAndLevel="", usecol=5, verbosity=1)
#' @export
lognetProbWrapper<-function(lognet, dfr, betweenColAndLevel, usecol, verbosity=0)
{
	if(! inherits(dfr, "dfrConversionProbs"))
	{
		catwif(verbosity >0, "Got non-dfrConversionProbs, so converting manually")
		dfr<-dfrConversionProbs(dfr, betweenColAndLevel)
	}
	if(missing(usecol))
	{
		if(ncol(lognet$beta) != 1) stop("lognetProbWrapper needs usecol if not 1 lambda lognet")
		usecol<-1
	}
	dfr$newformdata$betas<-lognet$beta[,usecol]
	keepVars<-abs(dfr$newformdata$betas)>0
	catwif(verbosity >0, "reducing from ", nrow(dfr$newformdata), "to", sum(keepVars), "variables/dummies")
	dfr$newformdata<-dfr$newformdata[keepVars,]

	dfr$newformdata$mustEqual<-rep(1, nrow(dfr$newformdata))
	dfr$newformdata$mustEqual[dfr$newformdata$isfact]<-dfr$newformdata$newlvls[dfr$newformdata$isfact]
	retval<-list(conversionProps=data.frame(
			fromCols=dfr$newformdata$repcols,
			newNames=dfr$newformdata$newcoln,
			mustEqual=dfr$newformdata$mustEqual,
			useBeta=dfr$newformdata$betas, stringsAsFactors=FALSE),
		originalLognet=lognet,
	  usedcol=usecol)
	class(retval)<-"lognetProbWrapper"
	return(retval)
}