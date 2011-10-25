#' Fit EMLasso to a complete dataset, creating a GLoMo for each lambda
#' 
#' Fit EMLasso to a complete dataset, creating a GLoMo for each lambda
#' 
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param dsconvprobs see \code{\link{dfrConversionProbs}}
#' @param lambdas see \code{\link{dfrConversionProbs}}
#' @param showPlot see \code{\link{findReasonableLambdaHelper}} (only relevant if no \code{lambdas} passed)
#' @param type.measure see \code{\link{findReasonableLambdaHelper}} (only relevant if no \code{lambdas} passed)
#' @param repsNeededForFirstOccurrence see \code{\link{findReasonableLambdaHelper}} (only relevant if no \code{lambdas} passed)
#' @param minNumHigher see \code{\link{findReasonableLambdaHelper}} (only relevant if no \code{lambdas} passed)
#' @param minNumLower see \code{\link{findReasonableLambdaHelper}} (only relevant if no \code{lambdas} passed)
#' @param maxNumLower see \code{\link{findReasonableLambdaHelper}} (only relevant if no \code{lambdas} passed)
#' @param betweenColAndLevel see \code{\link{dfrConversionProbs}}
#' @param \dots passed on to \code{\link{EMLasso.1l.lognet}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param logdir path to folder where logfiles (and results) of each repeat are stored
#' @param saveTempResults if \code{TRUE}, save the results of each (parallelized) 
#' @return to be looked into
#' @note If lambdas is not passed along or is \code{NULL}, a set of lambdas is used
#' 	by utilizing \code{\link{findReasonableLambdaHelper}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l.lognet}}
#' @keywords GLoMo EMLasso
#' @export
fullDataEMLassoAndGLoMo<-function(ds, out, dsconvprobs, lambdas, betweenColAndLevel="", 
	showPlot=TRUE, type.measure="auc", repsNeededForFirstOccurrence=3, minNumHigher = 20, 
	minNumLower = 20, maxNumLower = 30, ..., verbosity=0,
	logdir="./", saveTempResults=TRUE)
{
	if((missing(dsconvprobs)) || (is.null(dsconvprobs)))
	{
		catwif(verbosity > 0, "dsconvprobs was not passed along, so needed to recalculate it")
		dsconvprobs<-dfrConversionProbs(dfr=ds, betweenColAndLevel="")
	}
	else
	{
		if(betweenColAndLevel != dsconvprobs$betweenColAndLevel)
		{
			catwif(verbosity > 0, "betweenColAndLevel passed does not match the one in dsconvprobs. Will use this last one (", dsconvprobs$betweenColAndLevel, ").")
			betweenColAndLevel<-dsconvprobs$betweenColAndLevel
		}
	}
	catwif(verbosity > 0, "Complete the data once ")
	weightsName<-"fullDataEMLassoAndGLoMo_weights"
	orgriName<-"fullDataEMLassoAndGLoMo_orgri"
	completedData<-rCatsAndCntInDfr(dfr=ds, weightsName=weightsName,
		orgriName=orgriName, reweightPerRow=TRUE, verbosity=verbosity-1)
	completedWeights<-completedData[[weightsName]]
	completedOrgri<-completedData[[orgriName]]
	rmColsPos<-match(c(weightsName, orgriName), colnames(completedData))
	completedData<-completedData[,-rmColsPos]
	completedUseOut<-out[completedOrgri]
	
	if((missing(lambdas)) || (is.null(lambdas)))
	{
		catwif(verbosity > 0, "Needed to calculate the reasonable set of lambdas")
		rl<-findReasonableLambdaHelper(ds=completedData, out=completedUseOut, showPlot=showPlot,
			type.measure=type.measure, repsNeededForFirstOccurrence=repsNeededForFirstOccurrence, 
			..., verbosity=verbosity-1, minNumHigher=minNumHigher, minNumLower=minNumLower, 
			maxNumLower=maxNumLower)
		lambdas<-rl$allLambda[do.call(seq, as.list(range(rl$regionDfr$idx)))]
	}

	allrelevantparams<-c(list(dfr=ds, resp=out, lambda=lambdas, dfrconvprobs=dsconvprobs, verbosity=verbosity), list(...))
	params<-do.call(EMLasso.1l.lognet.param, allrelevantparams) #in fact I should be able to get this from e1l_param
	if(saveTempResults) savedir<-logdir else savedir<-NULL
	result<-run.parallel(dfr=ds, resp=out, lambda=lambdas, dfrconvprobs=dsconvprobs, ..., 
		paramcreationname="EMLasso.1l.lognet.param",
		functionname="EMLasso.1l.lognet", paramname="e1l_param", logdir=logdir,
		savedir=savedir, postprocessname=NULL, 
		loadLibsIfSfNotRunning=c("Matrix", "glmnet", "addendum", "NumDfr", "GLoMo", "EMLasso"))
	retval<-list(result=result, lambda=lambdas, params=params, logdir=logdir)
	class(retval)<-"EMLasso.lognet"
	return(retval)
}