#' Simple threshold selection for some sets of variables selected by EMLasso
#' 
#' Simple threshold selection for some sets of variables selected by EMLasso
#' 
#' @param cvob result of a call to \code{\link{crossValidate.EMLassoGLoMo}}
#' @param coefsUsed list holding vectors of names of columns (after conversion). Thresholds
#' 	are selected for each of the sets of variables.
#' @param reps number of repeats of imputation / crossvalidation
#' @param checkThres set of values between 0 and 1 where the TPR / FPR are evaluated
#' @param ds dataset (defaults to the one passed to \code{\link{EMLasso}})
#' @param out outcome vector (defaults to the one passed to \code{\link{EMLasso}})
#' @param wts weight vector per observation (defaults to the one passed to \code{\link{EMLasso}})
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}
#' @param \dots passed on to \code{\link{collectImputationModels}}
#' @param niceNames display names for each of the sets of variabels, to be used n the titles of the plots
#' @param newWindow if \code{TRUE} (not the default), a new window is created for the plots
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return a list, holding the following items:
#' @note Thresholds are selected by finding the highest TPR with a maximal FPR of 50%
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords fit logistic regression
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' iris.cpy<-randomNA(iris, n=0.1)
#' iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,  
#' 	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=0,
#' 	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
#' sfStop()
#' iris.cv.emlognet<-crossValidate(iris.emlognet, verbosity=0)
#' thresHolding(iris.cv.emlognet, coefsUsed=list(onlySepL=c("Sepal.Length"), onlySpecies=c("Speciesversicolor", "Speciesvirginica")), reps=10, verbosity=2)
#' @export
thresHolding<-function(cvob, coefsUsed=NULL, reps=500, checkThres=seq(0,1, length.out=101),
											 ds=cvob$glmnet.fit$result[[1]]$ds, out=cvob$glmnet.fit$result[[1]]$out, 
											 wts=rep(1, nrow(ds)), imputeDs2FitDsProperties=cvob$glmnet.fit$imputeDs2FitDsProperties, 
											  ..., niceNames=c(names(coefsUsed)), newWindow=FALSE, verbosity=0)
{
	if(is.null(coefsUsed))
	{
		coefsUsed<-lapply(c("lambda.min", "lambda.1se"), function(curs){
			tmp<-coef(cvob, s=curs)
			return(setdiff(rownames(tmp)[abs(as.vector(tmp)) > 0.00001], "(Intercept)"))
		})
	}
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(imputeDs2FitDsProperties)
	
	catwif(verbosity>0, "Will try to do thresholding for these coefficient sets:")
	printif(verbosity>0, coefsUsed)
	
	impData<-collectImputationModels(model=cvob$glmnet.fit, ds=ds, ..., useCombinedGLoMo=TRUE, verbosity=verbosity-1)
	useGLoMo<-impData$combinedGLoMo
	useReusable<-impData$reusableData
	
	catwif(verbosity>0, "Will start predicting now")
	predprobs.wcoef<-repeatedlyPredictOut(useGLoMo, ds=ds, out=out, reps=reps, varsets=coefsUsed, 
																				verbosity=verbosity-1, returnCoefs=TRUE, reusabledata=useReusable,
																				imputeDs2FitDsProperties=imputeDs2FitDsProperties)
	catwif(verbosity>0, "Done predicting -> show results")
	
	if(newWindow) windows()
	squareLikeLayoutForNGraphs(n=length(coefsUsed))
	ROC<-lapply(seq_along(coefsUsed), function(vsi){
		rv<-plotROCFromRepPredProb(obsrepprob=predprobs.wcoef$result[[vsi]], out=out, thres=checkThres, verbosity=verbosity-2)
		title(main=niceNames[vsi])
		abline(h=0.75, col="green", lty="dotted")
		abline(v=0.50, col="green", lty="dotted")
		oop<-rv[max(seq(nrow(rv))[rv[,"FPR"]<0.5]), ]
		cat("Performance for", niceNames[vsi], ".\n")
		print(oop)
		abline(h=oop["TPR"], col="purple", lty="dotted")
		abline(v=oop["FPR"], col="purple", lty="dotted")
		return(rv)
	})
	layout(1)
	
	rv<-list(
		coefsUsed=coefsUsed,
		predprobs.wcoef=predprobs.wcoef,
		useGLoMo=useGLoMo,
		useReusable=useReusable,
		ROC=ROC
	)
	return(rv)
}