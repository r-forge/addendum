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
	this.call = match.call()
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
			verbosity=verbosity-1, minNumHigher=minNumHigher, minNumLower=minNumLower, 
			maxNumLower=maxNumLower)
		lambdas<-rl$allLambda[do.call(seq, as.list(range(rl$regionDfr$idx)))]
	}

	catwif(verbosity > 0, "Collect parameters for actual run down lambdas.")
	allrelevantparams<-c(list(dfr=ds, resp=out, lambda=lambdas, dfrconvprobs=dsconvprobs, verbosity=verbosity), list(...))
	params<-do.call(EMLasso.1l.lognet.param, allrelevantparams) #in fact I should be able to get this from e1l_param
	if(saveTempResults) savedir<-logdir else savedir<-NULL
	catwif(verbosity > 0, "Will now start parallel run..")
	result<-run.parallel(dfr=ds, resp=out, lambda=lambdas, dfrconvprobs=dsconvprobs, verbosity=verbosity, 
		..., paramcreationname="EMLasso.1l.lognet.param",
		functionname="EMLasso.1l.lognet", paramname="e1l_param", logdir=logdir,
		savedir=savedir, postprocessname=NULL, 
		loadLibsIfSfNotRunning=c("Matrix", "glmnet", "addendum", "NumDfr", "GLoMo", "EMLasso"))
	catwif(verbosity > 0, "Done with parallel run, so collecting results and returning.")
	#retval<-list(result=result, lambda=lambdas, params=params, logdir=logdir)
	listOfLassoFits<-lapply(result, "[[", "lasso.fit")
	useBeta<-try(do.call(cBind, lapply(listOfLassoFits, "[[", "beta")))
	retval<-list(
		call=this.call,
		a0=try(sapply(listOfLassoFits, "[[", "a0")),
		beta=useBeta,
		lambda=lambdas,
		dev.ratio=try(sapply(listOfLassoFits, "[[", "dev.ratio")),
		nulldev=try(sapply(listOfLassoFits, "[[", "nulldev")),
		df=try(sapply(listOfLassoFits, "[[", "df")),
		dim=dim(useBeta),
		nobs=listOfLassoFits[[1]]$nobs,
		npasses=try(sum(sapply(listOfLassoFits, "[[", "npasses"))),
		offset=listOfLassoFits[[1]]$offset,
		jerr=try(sapply(listOfLassoFits, "[[", "jerr")), #this ends the glmnet items
		result=result,
		params=params, 
		logdir=logdir
	)
	class(retval)<-c("EMLasso.lognet", class(result[[1]]$lasso.fit))
	return(retval)
}