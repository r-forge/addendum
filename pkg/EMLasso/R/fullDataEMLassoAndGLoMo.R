#' Fit EMLasso to a complete dataset, creating a GLoMo for each lambda
#' 
#' Fit EMLasso to a complete dataset, creating a GLoMo for each lambda
#' 
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param dsconvprobs see \code{\link{dfrConversionProbs}}
#' @param lambdas see \code{\link{dfrConversionProbs}}
#' @param betweenColAndLevel see \code{\link{dfrConversionProbs}}
#' @param \dots passed on to \code{\link{findReasonableLambdaHelper}} if needed.
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return to be looked into
#' @note If lambdas is not passed along or is \code{NULL}, a set of lambdas is used
#' 	by utilizing \code{\link{findReasonableLambdaHelper}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l.lognet.cv}}
#' @keywords GLoMo EMLasso
#' @export
fullDataEMLassoAndGLoMo<-function(ds, out, dsconvprobs, lambdas, betweenColAndLevel="", ..., verbosity=0)
{
	if((missing(dsconvprobs)) || (is.null(dsconvprobs)))
	{
		catwif(verbosity > 0, "dsconvprobs was not passed along, so needed to recalculate it")
		rl<-dfrConversionProbs(dfr=ds, betweenColAndLevel="")
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
		rl<-findReasonableLambdaHelper(ds=completedData, out=completedUseOut, ..., verbosity=verbosity-1)
		lambdas<-rl$allLambda[do.call(seq, as.list(range(rl$regionDfr$idx)))]
	}
}