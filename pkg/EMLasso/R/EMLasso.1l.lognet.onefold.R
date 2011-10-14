#' One fold of crossvalidate EMLasso for 1 lambda
#' 
#' One fold of crossvalidate EMLasso for 1 lambda
#' 
#' @param curGroup number of the fold to do
#' @param groupNrPerRow vector of groupnumbers per row of \code{dfr}
#' @param dfr dataset (\code{\link{numdfr}} or \code{\link{data.frame}}) to fit it to
#' @param resp outcome vector
#' @param lambda the single lambda
#' @param nrOfSamplesPerMDRow For rows with missing data, how many rows to sample
#' @param minIt minimum number of iterations before convergence is possible
#' @param maxIt maximum number of iterations before convergence is automatically assumed
#' @param weightsName name that can be safely given to a weights column (should not conflict
#' with existing column names)
#' @param orgriName name that can be safely given to a original row index column (should not 
#' conflict with existing column names)
#' @param precalcGuidData (experimental) if \code{TRUE} an effort is made to reuse GuidData
#' @param dfrconvprobs dataset conversion properties (see \code{\link{dfrConversionProbs}})
#' @param reuseDebugLevel (for internal use only)
#' @param nrOfSamplesForCriteria similar to nrOfSamplesPerMDRow, but for final calculation of
#' validation criteria
#' @param calculateCriteria function to calculate criteria
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return An object of class "cvpart.emlasso", see the \code{actualfits} item of \code{\link{cv.1l.emlasso}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords EMLasso crossvalidate
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{nrs<-nrow(aDfr.MD)
#' uniqueGroupNrs<-seq(10)
#' groupNrPerRow<-similarSizeGroups(10, nobs=nrs, rand = TRUE)
#' eml1ffit<-EMLasso.1l.lognet.onefold(1, groupNrPerRow, aDfr.MD, y, lambda=0.05, 
#' 	nrOfSamplesPerMDRow=7, maxIt=20, minIt=10, weightsName="weights", orgriName="orgri",
#' 	precalcGuidData=FALSE, reuseDebugLevel=-1, 
#' 	nrOfSamplesForCriteria=7, calculateCriteria=calculateCriteria.EMLasso.1l.lognet,verbosity=10)}
#' eml1ffit<-eml17$actualfits[[1]]
#' @export
EMLasso.1l.lognet.onefold<-function(curGroup, groupNrPerRow, dfr, resp, lambda,
			nrOfSamplesPerMDRow, maxIt, minIt, weightsName, orgriName,
			precalcGuidData, dfrconvprobs, reuseDebugLevel, 
			nrOfSamplesForCriteria, calculateCriteria, verbosity=0)
{
	rowsToUseForFit<-which(groupNrPerRow!=curGroup)
	catwif(verbosity > 0, "fitting once")
	if(reuseDebugLevel > 1)
	{
		catwif(verbosity > 0, "reusing first fit from previous debug")
		onceFit<-.getEMLProp("onceFit")
	}
	else
	{
		deeperReuseDebugLevel<- -1 #maybe later allow reuse at deeper level
		if(reuseDebugLevel==-10) deeperReuseDebugLevel<- reuseDebugLevel
		onceFit<-EMLasso.1l.lognet(dfr=dfr, resp=resp, lambda=lambda,
			nrOfSamplesPerMDRow=nrOfSamplesPerMDRow, rowsToUseForFit=rowsToUseForFit,
			maxIt=maxIt, minIt=minIt, weightsName=weightsName, orgriName=orgriName,
			precalcGuidData=precalcGuidData, dfrconvprobs=dfrconvprobs,
			verbosity=verbosity-1, reuseDebugLevel=deeperReuseDebugLevel)
		if(reuseDebugLevel > -1) .setEMLProp("onceFit", onceFit)
	}
	catwif(verbosity > 0, "Finished fit, create validating sample.")
	otherRows<-which(groupNrPerRow==curGroup)
	
	if(reuseDebugLevel > 3)
	{
		catwif(verbosity > 0, "reusing validation sample from previous debug")
		valSample<-.getEMLProp("valSample")
	}
	else
	{
		valSample<-predict(object=onceFit$glomo, nobs=nrOfSamplesForCriteria,
			newdata=dfr, forrows=otherRows, returnRepeats=TRUE, verbosity=verbosity-1)
		valSample$resp<-rep(resp[otherRows], valSample$numRepPerRow)
		if(reuseDebugLevel > -1)
		{
			.setEMLProp("valSample", valSample)
		}
	}
	
	rv<-list(fitinfo=onceFit, valsample=valSample)
	if(! is.null(calculateCriteria))
	{
		catwif(verbosity > 0, "Finished fit, calculate criteria.")
		crits<-try(calculateCriteria(onceFit, valSample, verbosity=verbosity-1))
		if(inherits(crits, "try-error"))
		{
			catw("Error calculating criteria. will have to look into this.")
			catw("The current error is:")
			print(crits)
			catw("And the tracback:")
			traceback()
			catw("This error will be stored in the criteria field...")
		}
		rv$criteria<-crits
	}
	class(rv)<-"cvpart.emlasso"
	return(rv)	
}