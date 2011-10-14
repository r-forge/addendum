#' Fit EMLasso for 1 lambda
#' 
#' Fit EMLasso for 1 lambda
#' 
#' @param dfr dataset (\code{\link{numdfr}} or \code{\link{data.frame}}) to fit it to
#' @param resp outcome vector
#' @param lambda the single lambda
#' @param nrOfSamplesPerMDRow For rows with missing data, how many rows to sample
#' @param rowsToUseForFit Which of the rows of dfr/resp can be used for fitting the LASSO
#' @param minIt minimum number of iterations before convergence is possible
#' @param maxIt maximum number of iterations before convergence is automatically assumed
#' @param weightsName name that can be safely given to a weights column (should not conflict
#' with existing column names)
#' @param orgriName name that can be safely given to a original row index column (should not 
#' conflict with existing column names)
#' @param precalcGuidData (experimental) if \code{TRUE} an effort is made to reuse GuidData
#' @param dfrconvprobs dataset conversion properties (see \code{\link{dfrConversionProbs}})
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param reuseDebugLevel (for internal use only)
#' @return An object of class cv.1l.lognet. See \code{\link{cv.1l.emlasso}}
#' @note Contrary to full.glmnet.EM.fit, this function assumes the factors are already
#' correctly assigned
#' rowsToUseForFit should hold the row indices in dfr that can be used for fitting
#' the glmnet
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @references [PENDING]
#' @keywords EMLasso fit
#' @examples  data(emlcvfit, package="EMLasso")
#' \dontrun{emlfit<-EMLasso.1l.lognet(aDfr.MD, y, lambda=0.05, nrOfSamplesPerMDRow=7, verbosity=10)}
#' emlfit<-eml17$actualfits[[1]]$fitinfo
#' @export
EMLasso.1l.lognet<-function(dfr, resp, lambda, nrOfSamplesPerMDRow,
	rowsToUseForFit=seq(nrow(dfr)), maxIt=20,
	minIt=10, weightsName="weights", orgriName="orgri", precalcGuidData=TRUE,
	dfrconvprobs,	verbosity=0, reuseDebugLevel=-1)
{
	if(missing(dfrconvprobs))
	{
		catwif(verbosity>0, "dfrconvprobs was not passed along. Calculating it now.")
		dfrconvprobs<-dfrConversionProbs(dfr, betweenColAndLevel="")
	}
	numObsToUse<-length(rowsToUseForFit)
	numObs<-nrow(dfr)
	numVars<-ncol(dfr)
	levelslist<-allLevels(dfr)
	factCols<-findCatColNums(dfr)
	#numFacts<-sum(sapply(seq(dim(dfr)[2]), function(curcn){is.factor(dfr[1,curcn])})) #we know now that these are the starting cols
	numFacts<-length(factCols) 
	catwif(verbosity > 0, numObsToUse, "/", numObs, "obs,", numFacts, "factor vars,", numVars, "vars total.")

	#get an initial estimate of the GLoMo parameters:
	#First, 'randomly' complete the dataset:
	catwif(verbosity > 0, "random fill the dataset, dims before:", dim(dfr))
	if(reuseDebugLevel > 0)
	{
		catwif(verbosity > 0, "get the dataset back from previous debug, dims before:", dim(dfr))
		completedData<-.getEMLProp("completedData")
		completedWeights<-.getEMLProp("completedWeights")
		completedOrgri<-.getEMLProp("completedOrgri")
		completedUseForFit<-.getEMLProp("completedUseForFit")
		completedUseOut<-.getEMLProp("completedUseOut")
	}
	else
	{
		completedData<-rCatsAndCntInDfr(dfr=dfr, weightsName=weightsName,
			orgriName=orgriName, reweightPerRow=TRUE, verbosity=verbosity-1)
		completedWeights<-completedData[[weightsName]]
		completedOrgri<-completedData[[orgriName]]
		rmColsPos<-match(c(weightsName, orgriName), colnames(completedData))
		completedData<-completedData[,-rmColsPos]
		completedUseForFit<-completedOrgri %in% rowsToUseForFit
		completedUseOut<-(resp[completedOrgri])[completedUseForFit]

		if(reuseDebugLevel > -1)
		{
			.setEMLProp("completedData", completedData)
			.setEMLProp("completedWeights", completedWeights)
			.setEMLProp("completedOrgri", completedOrgri)
			.setEMLProp("completedUseForFit", completedUseForFit)
			.setEMLProp("completedUseOut", completedUseOut)
		}
	}
	
	catwif(verbosity > 0, "random fill the dataset, dims after:", dim(completedData))
	
	#get an initial estimate of the lasso parameters
	#only the rows that correspond to rowsToUseForFit are to be used
	catwif(verbosity > 0, "First Lasso fit.") #takes _a lot_ of time
	if(reuseDebugLevel > 2)
	{
		catwif(verbosity > 0, "Get first Lasso fit back from previous debug.")
		lasso.fit<-.getEMLProp("firstLasso.fit")
	}
	else
	{
		lasso.fit<-fit.lognet(dfr=completedData[completedUseForFit,], resp=completedUseOut,
			lambda=lambda, weights=completedWeights[completedUseForFit],
			verbosity=verbosity-1, dfrConvData=dfrconvprobs)
		if(reuseDebugLevel > -1)
		{
			.setEMLProp("firstLasso.fit", lasso.fit)
			.setEMLProp("lastLasso.fit", lasso.fit)
		}
	}
	coefs<-coef(lasso.fit)
	#str(coefs)
	#apparently, this holds a sparse matrix with 1 column
	coefs<-as.data.frame(as.matrix(t(coefs)))

	catwif(verbosity > 0, "First predictor model fit.")
	#actually fit the predictor model on this adjusted dataset
	#(would it make sense to calculate the uniqueIdentifiersPerRow up front? Probably not?)
	if(reuseDebugLevel > 1)
	{
		catwif(verbosity > 0, "Get first predictor model fit back from previous debug.")
		glomo<-.getEMLProp("firstGlomo")
	}
	else
	{
		glomo<-GLoMo(dfr=completedData, weights=completedWeights, verbosity=verbosity-1)
		if(reuseDebugLevel > -1)
		{
			.setEMLProp("firstGlomo", glomo)
			.setEMLProp("lastGlomo", glomo)
		}
	}

	if(reuseDebugLevel > 3)
	{
		catwif(verbosity > 0, "Get guid data back from previous debug.")
		guidDataOfOriginalDfr<-.getEMLProp("guidDataOfOriginalDfr")
		uidsPerRowOfOriginalDfr<-.getEMLProp("uidsPerRowOfOriginalDfr")
	}
	else
	{
		catwif(verbosity > 0, "Get guid data.")
		#note: we know there's no missing data in dfr now
		guidDataOfOriginalDfr<-getGuidData(glomo=glomo, dfr=dfr, guidPerObservation=NULL,
			verbosity=verbosity-1)
		uidsPerRowOfOriginalDfr<-guidDataOfOriginalDfr$guidPerObservation
		if(reuseDebugLevel > -1)
		{
			.setEMLProp("guidDataOfOriginalDfr", guidDataOfOriginalDfr)
			.setEMLProp("uidsPerRowOfOriginalDfr", uidsPerRowOfOriginalDfr)
		}
	}
	
	#Here starts the actual EM
	converged<-FALSE
	iterCount<-0
	while(! converged)
	{
		iterCount<-iterCount+1
		catwif(verbosity > 1, "iteration", iterCount)
		catwif(verbosity > 1, "->Generating missing data")
		
		#first, fill out the missing data
		valData<-LognetValidationData(glmnetmodel=lasso.fit, dfr=dfrconvprobs, outcomes=resp, verbosity=verbosity-2)
		if(reuseDebugLevel == -10) .setEMLProp("condpredparams", list(object=glomo, nobs=nrOfSamplesPerMDRow,
			dfr=dfr, validateFunction=validateFunction.lognet, guidDataOfOriginalDfr=guidDataOfOriginalDfr,
			otherData=valData, verbosity=verbosity-2))
		catwif(verbosity > 1, "->Before predict call")
		tmp<-predict.conditional.allrows.GLoMo(object=glomo, nobs=nrOfSamplesPerMDRow,
			dfr=dfr, validateFunction=validateFunction.lognet, guidDataOfOriginalDfr=guidDataOfOriginalDfr,
			otherData=valData, verbosity=verbosity-2)
		if(reuseDebugLevel > -1) .setEMLProp("tmp", tmp)
		if((!precalcGuidData) && (iterCount==1)) guidDataOfOriginalDfr<-uidsPerRowOfOriginalDfr #reuse as much as possible

		curData<-tmp$predicted
		curWeights<-rep(1/tmp$repsperrow, tmp$repsperrow)#now holds 1 weight per observation
		curResp<-rep(resp, tmp$repsperrow)
		curOrgri<-rep(seq_along(resp), tmp$repsperrow)
		curUseForFit<-curOrgri %in% rowsToUseForFit
		curUseOut<-(resp[curOrgri])[curUseForFit]

		#Given this data, fit
		#a) The lasso
		catwif(verbosity > 1, "->Fitting lasso")
		lasso.fit<-fit.lognet(curData[curUseForFit,], curUseOut,
			lambda=lambda, weights=curWeights[curUseForFit], verbosity=verbosity-2,
			dfrConvData=dfrconvprobs)
		if(reuseDebugLevel > -1) .setEMLProp("lastLasso.fit", lasso.fit)
		#b) The Predictor Model
		catwif(verbosity > 1, "->Fitting predictors model")
		if(precalcGuidData) prevglomo<-glomo #otherwise not necessary => waste of memory
		glomo<-GLoMo(dfr=curData, weights=curWeights, verbosity=verbosity-2)
		if(reuseDebugLevel > -1)  .setEMLProp("lastGlomo", glomo)

		catwif(verbosity > 1, "->Checking convergence")
		newcoefs<-coef(lasso.fit)
		newcoefs<-as.data.frame(as.matrix(t(newcoefs)))
		coefs<-rbind(coefs, newcoefs) #may not work if some factor levels are missing...
		converged<-checkConvergence.lognet(coefs, minIt, maxIt, verbosity = verbosity-2)
		if((verbosity > 1) & (converged)) catt("****Convergence in iteration", iterCount)
		if((!converged) && (precalcGuidData))
		{
			catwif(verbosity > 1, "Updating guid Data")
			guidDataOfOriginalDfr<-updateGuidData(oldglomo=prevglomo, newglomo=glomo,
				oldrowsused=tmp$glomorowsused, oldguiddata=guidDataOfOriginalDfr)
		}
		if(reuseDebugLevel > -1)  .setEMLProp("lastguidDataOfOriginalDfr", guidDataOfOriginalDfr)
	}
	#with the last lasso, refit a classical logistic regression.
	usedCols<-which(coef(lasso.fit)[-1] != 0) #also holds the intercept!!!
	logreg.fit<-fit.logreg(curData[curUseForFit,], resp=curUseOut,
		wts=curWeights[curUseForFit], verbosity=verbosity-1, useCols=usedCols,
		dfrConvData=dfrconvprobs)

	retval<-list(
		lasso.fit=lasso.fit,
		glomo=glomo,
		coefs=coefs,
		dfr=dfr,
		resp=resp,
		lambda=lambda,
		nrOfSamplesPerMDRow=nrOfSamplesPerMDRow,
		maxIt=maxIt,
		minIt=minIt,
		rowsToUseForFit=rowsToUseForFit,
		iterCount=iterCount,
		logreg.fit=logreg.fit)
	class(retval)<-"EMLasso.1l.lognet"
	return(retval)
}