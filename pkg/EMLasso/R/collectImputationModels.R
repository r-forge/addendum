#' Collect data from a model, needed for multiple imputation based on this model
#' 
#' Collect data from a model, needed for multiple imputation based on this model
#' 
#' @param model model fit
#' @param ds dataset for which imputation will need to happen
#' @param \dots implementation dependent. e.g. for \code{\link{EMLassoGLoMo}} should be a list
#' 	containig an item \code{useCombinedGLoMo} (will default to \code{TRUE});
#' for \code{predict.EMLassoGLoMoImputationData}: passed on to \code{\link{cv.glmnet}}. Note: 
#' 	the ones implied by \code{object} are not supported! e.g. \code{family} and \code{standardize} 
#' 	cannot be overridden
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @note aids to generalize crossvalidation
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords crossvalidate model
#' @export
collectImputationModels<-function(model, ds=model$result[[1]]$ds, ..., verbosity=0) UseMethod("collectImputationModels")
#' @rdname collectImputationModels
#' 
#' @aliases collectImputationModels.EMLassoGLoMo EMLassoGLoMoImputationData-class EMLassoGLoMoImputationData
#' @method collectImputationModels EMLassoGLoMo
#' @usage \method{collectImputationModels}{EMLassoGLoMo}(model, ds=model$result[[1]]$ds, useCombinedGLoMo=TRUE, ..., verbosity=0)
#' @param useCombinedGLoMo if \code{FALSE} (not the default), a distinct GLoMo is used for every lambda. 
#' 	Otherwise, the combined GLoMo (see \code{\link{combineGLoMos}}) is used for all lambdas.
#' @return \code{collectImputationModels.EMLassoGLoMo} will return an object of class "EMLassoGLoMoImputationData". 
#' 	This is a list with items:
#' \item{imputationParams }{the \dots passed along} 
#' \item{useCombinedGLoMo }{as passed along} 
#' \item{combinedGLoMo }{if relevant (\code{useCombinedGLoMo==TRUE}): the combined \code{\link{GLoMo}}}
#' \item{glomolist }{if relevant (\code{useCombinedGLoMo==FALSE}): list containing the \code{\link{GLoMo}} to be used for each lambda}
#' \item{reusableData }{either a single object of class "ReusableDataForGLoMoSampling" (see 
#' 	\code{\link{reusableDataForGLoMoSampling}}), or a list with one of them per lambda} 
#' \item{lambda }{taken from \code{model}} 
#' \item{family }{taken from \code{model}} 
#' \item{imputeDs2FitDsProperties }{taken from \code{model}} 
#' @seealso \code{\link{EMLasso}}, \code{\link{cv.glmnet}}
#' @keywords GLoMo EMLasso imputation
#' @export
collectImputationModels.EMLassoGLoMo<-function(model, ds=model$result[[1]]$ds, useCombinedGLoMo=TRUE, ..., verbosity=0)
{
	impMod<-list(imputationParams=list(...), useCombinedGLoMo=useCombinedGLoMo)
	if(useCombinedGLoMo)
	{
		if(exists("combinedGLoMo", model))
		{
			catwif(verbosity>0, "Reusing combined GLoMo across lambdas.")
			combinedGLoMo<-model$combinedGLoMo
		}
		else
		{
			catwif(verbosity>0, "Combining GLoMos across lambdas.")
			glomolist<-lapply(model$result, "[[", "predictorModel")
			combinedGLoMo<-combineGLoMos(listOfGLoMos=glomolist, verbosity=verbosity-5)
		}
		impMod$combinedGLoMo<-combinedGLoMo
		impMod$reusableData<-reusableDataForGLoMoSampling(glomo = combinedGLoMo, dfr = ds, verbosity = verbosity - 1)
	}
	else
	{
		impMod$glomolist<-lapply(model$result, "[[", "predictorModel")
		impMod$reusableData<-lapply(impMod$glomolist, reusableDataForGLoMoSampling, dfr = ds, verbosity = verbosity - 2)
	}
	impMod$lambda<-model$lambda
	impMod$family<-model$family
	impMod$imputeDs2FitDsProperties<-model$imputeDs2FitDsProperties
	impMod$varsPerLam<-apply(as.matrix(model$beta), 2, function(coefsForCurLam){rownames(model$beta)[abs(coefsForCurLam) > 0.0001]})
	#note: smallest number of vars (so highest lambda) comes first
	class(impMod)<-"EMLassoGLoMoImputationData"
	return(impMod)
}
#' @rdname collectImputationModels
#' 
#' @aliases predict.EMLassoGLoMoImputationData
#' @method predict EMLassoGLoMoImputationData
#' @usage \method{predict}{EMLassoGLoMoImputationData}(object, newdata, out, wts=rep(1, nrow(newdata)), type.measure="auc", actualPredictAndEvaluateFunction, unpenalized=FALSE, ..., verbosity=0)
#' @param object "EMLassoGLoMoImputationData" that holds the information to perform imputations
#' @param newdata dataset for which imputation needs to occur
#' @param out outcomes that will be used for evaluating the models
#' @param wts weights per observation (defaults to equal weights for all observations)
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param actualPredictAndEvaluateFunction function similar to the nonexported \code{.predAndEvalGLN}
#' 	function. Expected to support parameters: \code{useGLoMo}, \code{useReusable}, \code{useLambda}, 
#' 	\code{newdata}, \code{out}, \code{weights}, \code{type.measure}, \code{imputeDs2FitDsProperties}, \code{nobs},
#' 	\code{...}, \code{verbosity=0}. Should return a vector twice the length of the number of rows: first
#' 	all evaluated values, then all their sds. Note: if this is not specified, \code{.predAndEvalGLN} is used.
#' @param unpenalized if \code{TRUE} (not the default) a simple regression model is fit with the selected variables
#' @seealso \code{\link{EMLasso}}, \code{\link{cv.glmnet}}
#' @keywords GLoMo EMLasso
#' @export
predict.EMLassoGLoMoImputationData<-function(object, newdata, out, wts=rep(1, nrow(newdata)), type.measure="auc", actualPredictAndEvaluateFunction, unpenalized=FALSE, ..., verbosity=0)
{
	if(missing(actualPredictAndEvaluateFunction))
	{
		actualPredictAndEvaluateFunction<-.predAndEvalGLN
	}
	if(exists("combinedGLoMo", object))
	{
		useGLoMo<-object$combinedGLoMo
		useReusable<-object$reusableData
		if(unpenalized)
		{
			useLambda=object$varsPerLam
			partres<-.predAndEvalGLN(useGLoMo, useReusable, useLambda, newdata, out, weights=wts, type.measure, 
															 object$imputeDs2FitDsProperties, family=object$family, ..., 
															 fakeLam=object$lambda, verbosity=verbosity-1)
		}
		else
		{
			useLambda=object$lambda
			partres<-.predAndEvalGLN(useGLoMo, useReusable, useLambda, newdata, out, weights=wts, type.measure, 
															 object$imputeDs2FitDsProperties, family=object$family, ..., verbosity=verbosity-1)
		}
	}
	else
	{
		partres<-vapply(seq_along(object$lambda), function(lambdaindex){
			useGLoMo<-object$glomolist[[lambdaindex]]
			useReusable<-object$reusableData[[lambdaindex]]

			if(unpenalized)
			{
				useLambda=object$varsPerLam[[lambdaindex]]
				partres<-.predAndEvalGLN(useGLoMo, useReusable, useLambda, newdata, out, weights=wts, type.measure, 
																 object$imputeDs2FitDsProperties, family=object$family, ..., 
																 fakeLam=object$lambda[lambdaindex], verbosity=verbosity-1)
			}
			else
			{
				useLambda=object$lambda[lambdaindex]
				tempres<-.predAndEvalGLN(useGLoMo, useReusable, useLambda, newdata, out, weights=wts, type.measure, 
																 object$imputeDs2FitDsProperties, family=object$family, ..., verbosity=verbosity-1)
			}
			
			
			return(tempres)
		}, c(1.0,1.0))
		partres<-c(partres[1,],partres[2,])
	}
	return(partres)
	
}
#just a helper function
.predAndEvalGLN<-function(useGLoMo, useReusable, useLambda, newdata, out, weights, type.measure, imputeDs2FitDsProperties, nobs=1, fakeLam, ..., verbosity=0)
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
		curcv<-fit.logreg(ds=curds, out=useOut, wts=useWeights, verbosity=verbosity, useCols=useLambda, fakeLam=fakeLam,
											type.measure=type.measure, imputeDs2FitDsProperties=imputeDs2FitDsProperties, ...)
	}
	else
	{
		curcv<-fit.glmnet(ds=curds, out=useOut, lambda=useLambda, weights=useWeights, verbosity=verbosity, 
			standardize=FALSE, type.measure=type.measure, imputeDs2FitDsProperties=imputeDs2FitDsProperties, ...)
	}
	c(curcv$cvm, curcv$cvsd)
}