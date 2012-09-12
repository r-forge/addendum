#' Fit EMLasso for 1 lambda
#' 
#' Fit EMLasso for 1 lambda
#' 
#' @aliases EMLasso.1l EMLasso1l EMLasso1l-class
#' @param ds dataset (\code{\link{numdfr}} or \code{\link{data.frame}}) to fit it to
#' @param out outcome vector
#' @param lambda the single lambda
#' @param nrOfSamplesPerMDRow For rows with missing data, how many rows to sample. Defaults to 10.
#' @param rowsToUseForFit Which of the rows of dfr/out can be used for fitting the LASSO (int vector). Defaults to all rows.
#' @param firstTimeCompleter object supported by \code{\link{completeMarginal}} or of similar form
#' 	that will complete the dataset the first time. Defaults to \code{\link{marginalCompleter}} with default parameters.
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} object that will provide the conversion from imputed
#' 	dataset to one that is ready for fitting the predictor model
#' @param fitPredictor function that will perform the predictor model fit. Defaults to \code{\link{GLoMo}} and should be of this signature.
#' @param family see \code{\link{glmnet}}. Defaults to "binomial" (i.e. lasso penalized logistic regression).
#' @param convergenceChecker function that will check based on the coefficients over the repeats whether convergence has been achieved.
#' 	Defaults to \code{\link{convergenceCheckCreator}()}, so a wrapper around \code{\link{checkConvergence.glmnet}}. The function should
#' 	have parameters \code{coefs} and \code{verbosity=0}
#' @param postProcess function, like \code{\link{postProcessEMLasso1l}} (its default) and of that signature, to do some extra
#' 	work before returning the result of \code{EMLasso.1l}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param extraLog A function (by default one that does nothing) that will be used in 
#' each iteration to perform extra logging
#' @return An object of class EMLasso1l. This is a list with the following items:
#' \itemize{
#' 	\item \code{lasso.fit}: glmnet object
#' 	\item \code{predictorModel}: final predictor fit (typically of class GLoMo)
#' 	\item \code{coefs}: coefs for all iterations (rows) and dummycoded columnames (columns), incl. (intercept).
#' 	\item \code{ds}: original dataset passed along
#' 	\item \code{out}: outcome variable (1 for each row in ds)
#' 	\item \code{lambda}: 1 lambda value
#' 	\item \code{nrOfSamplesPerMDRow}: how many imputations per row with missing data
#' 	\item \code{convergence}: return value of last call to \code{convergenceChecker}. Will typically contain
#' 	 	information like \code{minIt} and \code{maxIt}.
#' 	\item \code{rowsToUseForFit}: which of the rows in ds was used to fit the lasso 
#' 	\item \code{iterCount}: how many iterations occurred before convergence / maxIt
#' 	\item \code{imputeDs2FitDsProperties}: as passed in, but first fed to \code{\link{imputeDs2FitDsProps}}
#' 	\item any other items added by \code{postProcess}
#' }
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @references [PENDING]
#' @keywords EMLasso fit
#' @examples aDfr<-generateTypicalIndependentDfr(numCat=10, numCnt=10, numObs=100, catProbs=rep(1/3,3),
#' rcnt=typicalRandomNorm, doShuffle=TRUE, verbosity=1)
#' 
#' outlins<- -mean(aDfr$cnt1)+aDfr$cnt1+2*(aDfr$cat1=="b")
#' outprobs<-expit(outlins)
#' y<-factor(sapply(outprobs, function(prob){sample(c("no", "yes"), 1, prob=c(1-prob,prob))}))
#' 
#' aDfr.MD<-randomNA(aDfr, 0.01, verbosity=1)
#' rlh<-findReasonableLambdaHelper(aDfr, y, verbosity=10)
#' aLam<-rlh$regionDfr[2,"lambda"]
#' emlfit1<-EMLasso.1l(aDfr.MD, y, lambda=aLam, nrOfSamplesPerMDRow=7,
#' 	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10), verbosity=10)
#' @export
EMLasso.1l<-function(ds, out, lambda, nrOfSamplesPerMDRow=10,
	rowsToUseForFit=seq(nrow(ds)), firstTimeCompleter=marginalCompleter(),
	imputeDs2FitDsProperties=normalImputationConversion(),	
	fitPredictor=GLoMo, #GLoMo(dfr, weights, verbosity)
	family="binomial", convergenceChecker=convergenceCheckCreator(),
	postProcess=postProcessEMLasso1l, verbosity=0, extraLog=function(...){})
{
	if(is.data.frame(ds))
	{
		catwif(verbosity > 0, "ds was passed in as a data.frame. For performance reasons, it will now be converted to numdfr first!")
		ds<-as.numdfr(ds)
	}
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(object=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity)
	numObsToUse<-length(rowsToUseForFit)
	numObs<-nrow(ds)
	numVars<-ncol(ds)
	levelslist<-allLevels(ds)
	factCols<-findCatColNums(ds)
	numFacts<-length(factCols) 
	catwif(verbosity > 0, numObsToUse, "/", numObs, "obs,", numFacts, "factor vars,", numVars, "vars total.")

	#get an initial estimate of the GLoMo parameters:
	#First, 'randomly' complete the dataset:
	catwif(verbosity > 0, "random fill the dataset, dims before:", dim(ds))
	imputedData<-completeMarginal(object=firstTimeCompleter,ds=ds, out=out, rowsToUse=rowsToUseForFit, verbosity=verbosity-2)
	
	catwif(verbosity > 0, "random fill the dataset, dims after:", dim(imputedData$ds))
	
	#get an initial estimate of the lasso parameters
	#only the rows that correspond to rowsToUseForFit are to be used
	catwif(verbosity > 0, "First Lasso fit.") #takes _a lot_ of time
	lasso.fit<-fit.glmnet(ds=imputedData$ds[imputedData$completedUse,], out=subsetFirstDim(imputedData$out,imputedData$completedUse), 
		lambda=lambda, weights=imputedData$weights[imputedData$completedUse],
		verbosity=verbosity-1, imputeDs2FitDsProperties=imputeDs2FitDsProperties, family=family)
	coefs<-coef(lasso.fit)
	#apparently, this holds a sparse matrix with 1 column
	coefs<-as.data.frame(as.matrix(t(coefs)))

	catwif(verbosity > 0, "First predictor model fit.")
	#actually fit the predictor model on this adjusted dataset
	#(would it make sense to calculate the uniqueIdentifiersPerRow up front? Probably not?)
	predictorModel<-fitPredictor(dfr=imputedData$ds, weights=imputedData$weights, verbosity=verbosity-1)

	reusableForSampling<-predictorModelSamplingReusables(predictorModel=predictorModel, iterCount=0, 
		previousReusables=NULL, ds=ds, verbosity=verbosity-1)
	extraLog(reusableForSampling)
	
	#Here starts the actual EM
	convergence<-list(converged=FALSE) #make this similar in form to convergenceChecker result
	iterCount<-0
	while(! convergence$converged)
	{
		iterCount<-iterCount+1
		catwif(verbosity > 1, "iteration", iterCount)
		catwif(verbosity > 1, "->Generating missing data")
		
		#first, fill out the missing data
		reusableForvalidation<-outcomeModelValidationReusables(outcomeModel=lasso.fit, ds=ds, out=out, 
			imputeDs2FitDsProperties=imputeDs2FitDsProperties, verbosity=verbosity-2)
		catwif(verbosity > 1, "->Before predict call")
		
		curData<-sampleConditional(predictorModel=predictorModel, outcomeModel=lasso.fit, 
			nrOfSamplesPerMDRow=nrOfSamplesPerMDRow, ds=ds, out=out, rowsToUseForFit=rowsToUseForFit,
			reusableForSampling=reusableForSampling, 
			reusableForvalidation=reusableForvalidation, verbosity=verbosity-2)
		
		extraLog(curData, "Imputed data", ds, out)
		
		reusableForSampling<-predictorModelSamplingReusables(predictorModel=predictorModel, iterCount=iterCount, 
			previousReusables=reusableForSampling, ds=ds, verbosity=verbosity-1)
		extraLog(reusableForSampling)

		#Given this data, fit
		#a) The lasso
		catwif(verbosity > 1, "->Fitting lasso")
		lasso.fit<-fit.glmnet(ds=curData$ds[curData$useForFit,], out=curData$useOut, 
			lambda=lambda, weights=curData$weights[curData$useForFit],
			verbosity=verbosity-2, imputeDs2FitDsProperties=imputeDs2FitDsProperties, family=family)
		extraLog(lasso.fit)
		
		#b) The Predictor Model
		catwif(verbosity > 1, "->Fitting predictors model")
		predictorModel<-fitPredictor(dfr=curData$ds, weights=curData$weights, verbosity=verbosity-2)
		extraLog(predictorModel)
		
		catwif(verbosity > 1, "->Checking convergence")
		newcoefs<-coef(lasso.fit)
		newcoefs<-as.data.frame(as.matrix(t(newcoefs)))
		coefs<-rbind(coefs, newcoefs) #may not work if some factor levels are missing...
		
		convergence<-convergenceChecker(coefs, verbosity = verbosity-2)
		#converged<-checkConvergence.lognet(coefs, minIt, maxIt, verbosity = verbosity-2)
		
		if((verbosity > 1) & (convergence$converged)) catt("****Convergence in iteration", iterCount)
	}
	retval<-list(
		lasso.fit=lasso.fit,
		predictorModel=predictorModel,
		coefs=coefs,
		ds=ds,
		out=out,
		lambda=lambda,
		nrOfSamplesPerMDRow=nrOfSamplesPerMDRow,
		convergence=convergence,
		rowsToUseForFit=rowsToUseForFit,
		iterCount=iterCount,
		imputeDs2FitDsProperties=imputeDs2FitDsProperties)
	class(retval)<-"EMLasso1l"
	
	catwif(verbosity > 0, "Postprocessing")
	retval<-postProcess(outcomeModel=lasso.fit, retval=retval, lastData=curData, 
		imputeDs2FitDsProperties=imputeDs2FitDsProperties, reusableForSampling=reusableForSampling, 
		reusableForvalidation=reusableForvalidation, verbosity=verbosity-1)
	return(retval)
}