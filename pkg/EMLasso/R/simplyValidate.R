#' Validate a model (to its original data)
#' 
#' Validate a model (to its original data)
#' 
#' @param model model fit
#' @param \dots for flexibility in 'derived' implementation
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @note aids to generalize validation
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords validate model
#' @export
simplyValidate<-function(model, ..., verbosity=0) UseMethod("simplyValidate")

#' @rdname simplyValidate
#' 
#' @aliases simplyValidate.EMLassoGLoMo sv.EMLassoGLoMo-class sv.EMLasso.lognet
#' @method simplyValidate EMLassoGLoMo
#' @usage \method{simplyValidate}{EMLassoGLoMo}(model, ds=model$result[[1]]$ds, out=model$result[[1]]$out, wts=rep(1, nrow(ds)), imputeDs2FitDsProperties=model$imputeDs2FitDsProperties, imputations=10, ..., type.measure="auc", keepResultPerLambda=FALSE, nobs=1, unpenalized=FALSE, verbosity=0)
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param wts vector of weights (defaults to equal weights for all rows)
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} object that will provide the conversion from imputed
#' 	dataset to one that is ready for fitting the predictor model
#' @param imputations Number of multiple imputations on the complete dataset (defaults to 10)
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param keepResultPerLambda if \code{TRUE} (not the default), the individual results
#' 	from the \code{simplyValidate.EMLasso1l} are also returned in an extra item
#' 	\code{resultPerLambda}
#' @param nobs how many observations are simulated for each row with missing data
#' @param unpenalized if \code{TRUE} (not the default) a simple regression model is fit with the selected variables
#' @return object that has as class: "sv." pasted before the class of \code{model}. Normally, \code{model} will
#' 	will be the return value of \code{\link{EMLasso}}, so this result is mainly the same as a \code{\link{cv.glmnet}}.
#' The added/altered items are:
#' \item{glmnet.fit}{is now the \code{model} passed in, so has more classes besides "glmnet" (e.g. "EMLasso")} 
#' \item{resultPerLambda }{matrix with one column per imputation. The top rows are the estimates for the criterion per
#' 	lambda, below that are their SD estimates. Not present if \code{keepResultPerLambda=FALSE}}
#' @seealso \code{\link{EMLasso}}, \code{\link{cv.glmnet}}
#' @keywords GLoMo EMLasso
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' iris.cpy<-randomNA(iris, n=0.1)
#' iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,  
#' 	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
#' 	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
#' sfStop()
#' iris.sv.emlognet<-simplyValidate(iris.emlognet, verbosity=2)
#' @export
simplyValidate.EMLassoGLoMo<-function(model, ds=model$result[[1]]$ds, out=model$result[[1]]$out, 
																		 wts=rep(1, nrow(ds)), imputeDs2FitDsProperties=model$imputeDs2FitDsProperties, imputations=10, 
																		 ..., type.measure="auc", keepResultPerLambda=FALSE, nobs=1, unpenalized=FALSE, verbosity=0)
{
	lambda<-model$lambda
	impData<-collectImputationModels(model=model, ds=ds, ..., verbosity=verbosity-1)
	partres<-vapply(seq(imputations), function(i){
		catwif(verbosity > 1, "Imputation", i, "/", imputations)
		predict(impData, newdata=ds, out=out, wts=wts, type.measure=type.measure, 
						actualPredictAndEvaluateFunction=.predAndEvalGLNS, nobs=nobs, unpenalized=unpenalized, verbosity=verbosity-1)
	}, rep(1.0, length(lambda)*2))
	#one row per lambda, 1 col per repetition
	cvms<-partres[seq_along(lambda),]
	cvsds<-partres[-seq_along(lambda),]
	#now use MI formulas per lambda
	cvm<-rowMeans(cvms, na.rm = TRUE)
	D<-imputations
	cvsd<-cvm
	cvwithinvar<-cvm
	cvbetweenvar<-cvm
	for(lami in seq_along(lambda))
	{
		cvwithinvar[lami]<-mean((cvsds[lami,])^2, na.rm = TRUE)
		cvbetweenvar[lami]<-var(cvms[lami,], na.rm = TRUE)
	} #calculate variance so far
	cvsd<-sqrt(cvwithinvar + (D+1)/D*cvbetweenvar)
	
	nz<-try(sapply(predict(model, type = "nonzero"), length))
	out<-list(
		lambda=lambda,
		cvm=cvm,
		cvsd=cvsd,
		cvup=try(cvm+cvsd),
		cvlo=try(cvm-cvsd),
		nzero=nz,
		glmnet.fit=model,
		cvwithinvar=cvwithinvar,
		cvbetweenvar=cvbetweenvar
	)
	cvsgn<- -2*(type.measure == "auc")+1
	lamin<-getmin(lambda, cvsgn*cvm, cvsd)
	obj = c(out, as.list(lamin))
	if(keepResultPerLambda)
	{
		obj<-c(obj, list(resultPerLambda=partres))
	}
	class(obj)<-paste("cv", class(model), sep=".")
	return(obj)
}

#just a helper function
.predAndEvalGLNS<-function(useGLoMo, useReusable, useLambda, newdata, out, weights, type.measure, imputeDs2FitDsProperties, nobs=1, fakeLam, ..., verbosity=0)
{
	curds<-predict(useGLoMo, newdata=newdata, reusabledata=useReusable, nobs=nobs, verbosity=verbosity, returnRepeats=TRUE)
	curreps<-curds$numRepPerRow
	curds<-curds$predicted
	extreps<-rep(seq_along(curreps), curreps)
	
	useWeights<-(weights / curreps)[extreps]
	useOut<-out[extreps]
	
	if(is.list(useLambda) || is.character(useLambda))
	{
		#this means that we wanted to fit regular, unpenalized logistic regression instead
		curln<-fit.logreg(ds=curds, out=useOut, wts=useWeights, verbosity=verbosity-1, useCols=useLambda, fakeLam=fakeLam,
											imputeDs2FitDsProperties=imputeDs2FitDsProperties, ...)
	}
	else
	{
		curln<-fit.glmnet(ds=curds, out=useOut, lambda=useLambda, weights=useWeights, verbosity=verbosity-1, 
											standardize=FALSE, imputeDs2FitDsProperties=imputeDs2FitDsProperties, ...)
	}
	
	imputeDs2FitDsProperties <- imputeDs2FitDsProps(object = imputeDs2FitDsProperties, 
																									ds = curds, verbosity = verbosity - 5)
	dfr.mat <- imputeDs2FitDs(conversionData = imputeDs2FitDsProperties, 
														ds = curds, verbosity = verbosity - 5)
	
	predprobs<-predict(curln, newx=dfr.mat, type="response")
	
	cvres<-aucLikeLognet(predictedprobabilities=predprobs, out=useOut, wts=useWeights)
	c(cvres$cvm, cvres$cvsd)
}
