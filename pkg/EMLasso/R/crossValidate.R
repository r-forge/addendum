#' Crossvalidate a model
#' 
#' Crossvalidate a model
#' 
#' @param model model fit
#' @param \dots for flexibility in 'derived' implementation
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @note aids to generalize crossvalidation
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords crossvalidate model
#' @export
crossValidate<-function(model, ..., verbosity=0) UseMethod("crossValidate")

#' @rdname crossValidate
#' 
#' @aliases crossValidate.EMLasso.1l.lognet cv.EMLasso.1l.lognet-class cv.EMLasso.1l.lognet
#' @method crossValidate EMLasso.1l.lognet
#' @usage \method{crossValidate}{EMLasso.1l.lognet}(model, ds=model$dfr, out=model$resp, glomo=model$glomo,wts, dsconvprobs, needPredict=0, betweenColAndLevel="",..., verbosity=0)
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param glomo \code{\link{GLoMo}} object to use as predictor model
#' @param wts vector of weights (defaults to equal weights for all rows)
#' @param dsconvprobs see \code{\link{dfrConversionProbs}}
#' @param needPredict If \code{> 0}, the number of rows that is predicted from the \code{GLoMo}
#' 	in \code{model} for rows with missing data in \code{ds}. I \code{<0} then crossvalidation
#' 	happens in a multiple imputation fashion (each time imputing only once, but doing so 
#' 	repeatedly)
#' @param betweenColAndLevel see \code{\link{dfrConversionProbs}}
#' @return object of type "cv.EMLasso.1l.lognet":
#' \item{cv.logreg }{list of \code{\link{cv.1l.emlasso.reduced}} objects per lambda} 
#' \item{ds }{as passed in or reduced if predicted}
#' \item{out }{as passed in or extended if predicted}
#' \item{wts }{as passed in or extended if predicted}
#' \item{fromLambda }{as passed in, the lambda that came from the original model}
#' @seealso \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}}
#' @keywords GLoMo EMLasso
#' @export
crossValidate.EMLasso.1l.lognet<-function(model, ds=model$dfr, out=model$resp, 
	glomo=model$glomo, wts, dsconvprobs, needPredict=0, betweenColAndLevel="",
	type.measure="auc", ..., verbosity=0)
{
	if(missing(dsconvprobs))
	{
		catwif(verbosity>0, "dsconvprobs was not passed along. Calculating it now.")
		dsconvprobs<-dfrConversionProbs(ds, betweenColAndLevel=betweenColAndLevel)
	}
	if(dsconvprobs$betweenColAndLevel != betweenColAndLevel)
	{
		catwif(verbosity>0, "Passed along betweenColAndLevel does not match the one in dsconvprobs. This last one (", dsconvprobs$betweenColAndLevel, ") will be used.")
		betweenColAndLevel<-dsconvprobs$betweenColAndLevel
	}
	useVarIndexes<-as.vector(unlist(predict(model$lasso.fit, type="nonzero")))
	useVarNames<-rownames(model$lasso.fit$beta)[useVarIndexes]
	catwif(verbosity > 0, "For lambda ", model$lambda, ", use variables: ", useVarNames)
	
	if(needPredict>0)
	{
		newdta<-predict(glomo, nobs=needPredict, newdata=ds, returnRepeats=TRUE, 
			verbosity=verbosity-1)
		orgds<-ds
		ds<-newdta$predicted
		if(verbosity >5)
		{
			catw("dim of predicted dataset: ", dim(ds))
			catw("repeats (length=", length(newdta$numRepPerRow), "):")
			print(newdta$numRepPerRow)
			catw("length of out=", length(out))
		}
		out<-rep(out, newdta$numRepPerRow)
		wtfct<-rep(1/newdta$numRepPerRow, newdta$numRepPerRow)
		if(missing(wts))
		{
			catwif(verbosity >5, "no wts was passed in")
			wts<-wtfct
		}
		else
		{
			catwif(verbosity >5, "length of wts passed in=", length(wts))
			wts<-rep(wts, newdta$numRepPerRow)*wtfct
		}
	}
	else if(needPredict==0)
	{
		if(missing(wts))
		{
			wts<-rep(1, nrow(ds))
		}
	}
	
	if(needPredict>=0)
	{
		logregres<-try(cv.logreg(dfr=ds, resp=out, wts=wts, verbosity=verbosity-1, useCols=useVarNames, 
			dfrConvData=dsconvprobs, type.measure=type.measure, ...))
		if(needPredict>0)
		{
			#make the predicted ds object less memory intensive
			ds<-reduce(ds, orgdfr=orgds, repsperrow=newdta$numRepPerRow)
		}
	}
	else
	{
		logregres<-try(cv.MI.logreg(glomo=glomo, ds=ds, out=out, useVarNames=useVarNames, 
			reps=-needPredict, dsconvprops=dsconvprobs, lambda=model$lambda, 
			useAsGlmnetFit=model$lasso.fit, ..., verbosity=verbosity-1))
	}
	retval<-list(cv.logreg=logregres, ds=ds, out=out, wts=wts, fromLambda=model$lambda)
	class(retval)<-"cv.EMLasso.1l.lognet"
	return(retval)
}
		 
#' @rdname crossValidate
#' 
#' @aliases crossValidate.EMLasso.lognet cv.EMLasso.lognet-class cv.EMLasso.lognet
#' @method crossValidate EMLasso.lognet
#' @usage \method{crossValidate}{EMLasso.lognet}(model, ds=model$result[[1]]$dfr, out=model$result[[1]]$resp, wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0, betweenColAndLevel="",..., type.measure="auc", keepResultPerLambda=FALSE, useCombinedGLoMo=FALSE, verbosity=0)
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param keepResultPerLambda if \code{TRUE} (not the default), the individual results
#' 	from the \code{crossValidate.EMLasso.1l.lognet} are also returned in an extra item
#' 	\code{resultPerLambda}
#' @param useCombinedGLoMo if \code{FALSE} (default), a distinct GLoMo is used for every lambda. 
#' 	Otherwise, the combined GLoMo is used for all lambdas.
#' @return object of type "cv.EMLasso.lognet". This is mainly the same as a \code{\link{cv.glmnet}}.
#' The added/altered items are:
#' \item{glmnet.fit }{is now the model passed in, so of class "EMLasso.lognet", besides "glmnet"} 
#' \item{resultPerLambda }{list of "cv.EMLasso.1l.lognet" objects per lambda. Not present if \code{keepResultPerLambda=FALSE}}
#' @seealso \code{\link{EMLasso.1l.lognet}}, \code{\link{cv.logreg}}, \code{\link{cv.glmnet}}
#' @keywords GLoMo EMLasso
#' @export
crossValidate.EMLasso.lognet<-function(model, ds=model$result[[1]]$dfr, out=model$result[[1]]$resp, 
	wts=rep(1, nrow(ds)), dsconvprobs, needPredict=0, betweenColAndLevel="",..., type.measure="auc", 
	keepResultPerLambda=FALSE, useCombinedGLoMo=FALSE, verbosity=0)
{
	if(missing(dsconvprobs))
	{
		catwif(verbosity>0, "dsconvprobs was not passed along. Calculating it now.")
		dsconvprobs<-dfrConversionProbs(ds, betweenColAndLevel=betweenColAndLevel)
	}
	if(dsconvprobs$betweenColAndLevel != betweenColAndLevel)
	{
		catwif(verbosity>0, "Passed along betweenColAndLevel does not match the one in dsconvprobs. This last one (", dsconvprobs$betweenColAndLevel, ") will be used.")
		betweenColAndLevel<-dsconvprobs$betweenColAndLevel
	}
	if(useCombinedGLoMo)
	{
		catwif(verbosity>0, "Combining GLoMos across lambdas.")
		glomolist<-lapply(model$result, "[[", "glomo") #each item of model$result is of class "EMLasso.1l.lognet"
		combinedGLoMo<-combineGLoMos(listOfGLoMos=glomolist, verbosity=verbosity-5)
		partres<-lapply(model$result, crossValidate, ds=ds, out=out, 
			glomo=combinedGLoMo, wts=wts, dsconvprobs=dsconvprobs,
			needPredict=needPredict, ..., type.measure=type.measure, verbosity=verbosity-1)
	}
	else
	{
		partres<-lapply(model$result, crossValidate, ds=ds, out=out, 
			wts=wts, dsconvprobs=dsconvprobs,
			needPredict=needPredict, ..., type.measure=type.measure, verbosity=verbosity-1)
	}
	cvlogreglist<-lapply(partres, "[[", "cv.logreg")
	
	lambda<-model$lambda
	cvm<-as.vector(unlist(try(sapply(cvlogreglist, "[[", "cvm"))))
	cvsd<-as.vector(unlist(try(sapply(cvlogreglist, "[[", "cvsd"))))
	nz<-try(sapply(predict(model, type = "nonzero"), length))
	out<-list(
		lambda=lambda,
		cvm=cvm,
		cvsd=cvsd,
		cvup=try(cvm+cvsd),
		cvlo=try(cvm-cvsd),
		nzero=nz,
		glmnet.fit=model
	)
  lamin<-try(if (type.measure == "auc") 
      getmin(lambda, -cvm, cvsd)
  else getmin(lambda, cvm, cvsd))
  obj = c(out, as.list(lamin))
	if(keepResultPerLambda)
	{
		obj<-c(obj, list(resultPerLambda=partres))
	}
	class(obj)<-c("cv.EMLasso.lognet", "cv.glmnet")
	return(obj)
}

#' @rdname crossValidate
#' 
#' @param varsets list of character vectors holding the variables (names) to be checked
#' @param reps how many times does imputation have to be repeated?
#' @param nfolds number of folds for crossvalidation
#' @param dsconvprops see \code{dsconvprobs} (need to work on universal and correct naming...)
#' @return List of the same length as \code{varsets} (unless it was length 1, then the first 
#' 	object is simply returned). Each item is a matrix with one row for each row in \code{ds}
#' 	and one column per \code{reps}, and holds the predicted probability in a crossvalidation.
#' @keywords GLoMo EMLasso crossvalidate
#' @export
repeatedlyPredictOut<-function(glomo, ds, out, varsets, reps=10, nfolds=10, dsconvprops=NULL, ..., verbosity=0)
{
	if((missing(dsconvprops)) || (is.null(dsconvprops)))
	{
		catwif(verbosity > 0, "dsconvprops was not passed along, so needed to recalculate it")
		dsconvprops<-dfrConversionProbs(dfr=ds, betweenColAndLevel="")
	}
	
	catwif(verbosity > 0, "Create reusable data for GLoMo sampling up front...")
  reusabledata <- reusableDataForGLoMoSampling(glomo = glomo, dfr = ds, verbosity = verbosity - 10)
	
	result<-replicate(length(varsets), matrix(NA, nrow=nrow(ds), ncol=reps), simplify=FALSE)
	for(currep in seq(reps))
	{
		catwif(verbosity > 1, "***repeat", currep, "/", reps)
		grps<-similarSizeGroups(ngroups=nfolds, nobs=nrow(ds), rand=TRUE)
		curds<-predict(glomo, newdata=ds, reusabledata = reusabledata, verbosity=verbosity-5)
		for(curfld in seq(nfolds))
		{
			catwif(verbosity > 2, "****fold", curfld, "/", nfolds)
			fitds<-curds[grps!=curfld,]
			fitout<-out[grps!=curfld]
			valrows<-grps==curfld
			valds<-factorsToDummyVariables(curds[valrows,], dfrConvData=dsconvprops)
			for(vsi in seq_along(varsets))
			{
				catwif(verbosity > 3, "*****varset", vsi, "/", length(varsets))
				curUseVars<-varsets[[vsi]]
				try({#sometimes this goes wrong...
					curfit<-fit.logreg(dfr=fitds, resp=fitout, verbosity=verbosity-5, useCols=curUseVars, outName="out", dfrConvData=dsconvprops, ...)
					useCols<-rownames(curfit$beta)
					#catwif(verbosity > 3, "Effectively used columns:", useCols)
					valdscurvarset<-valds[,useCols]
					curpreds<-predict(curfit, newx=valdscurvarset, type="response") #predicted probability
					result[[vsi]][valrows,currep]<-curpreds
				})
			}
		}
	}
	if(length(result)==1) result<-result[[1]]
	return(result)
}

#' @rdname crossValidate
#' 
#' @param reppredprob one of the matrices as return by \code{repeatedlyPredictOut}
#' @return named vector of length 2, holding the "AUC" and the "AUCSD"
#' @keywords GLoMo EMLasso crossvalidate
#' @export
repeatedPredictedProbAUC<-function(reppredprob, out, verbosity=0)
{
	if(is.factor(out))
	{
		out<-as.integer(out)
	}
	if(! is.logical(out))
	{
		out<- out > 1
	}
	#note: this is in fact multiple imputation!
	#As such, we can use the formulas on p86 of "Statistical Analysis with Missing Data" here
	repres<-apply(reppredprob, 2, calcAUC.Binary, trueOnes=out, includeSE=TRUE)
	AUCs<-sapply(repres, "[[", "AUC")
	AUCVars<-sqrt(sapply(repres, "[[", "varAUC"))
	
	theAUC<-mean(AUCs)
	D<-length(AUCs)
	avgWithinVar<-mean(AUCVars)
	betwImpVar<-var(AUCs)
	totalVar<-avgWithinVar + (D+1)/D*betwImpVar
	c(AUC=theAUC, AUCSD=sqrt(totalVar))
}

#' @rdname crossValidate
#' 
#' @param useAsGlmnetFit object that can be used for the \code{glmnet.fit} item in the return value
#' @return object of classes "cv.MI.logreg", "cv.glmnet" and "cv.lognet". Has exactly the items of
#' 	a \code{\link{cv.glmnet}} object
#' @keywords GLoMo EMLasso crossvalidate
#' @export
cv.MI.logreg<-function(glomo, ds, out, useVarNames, reps, dsconvprops, lambda, useAsGlmnetFit, ..., verbosity=0)
{
	preds<-repeatedlyPredictOut(glomo=glomo, ds=ds, out=out, varsets=list(useVarNames), reps=reps, 
		..., dsconvprops=dsconvprops, verbosity=verbosity-1)
	auc<-repeatedPredictedProbAUC(preds, out=out, verbosity=verbosity-1)
	aucsd<-auc["AUCSD"]
	aucsd<-auc["AUC"]
	logregres<-list(lambda=lambda, cvm=auc, cvsd=aucsd, cvup=auc+aucsd,
		cvlo=auc-aucsd, nzero=length(useVarNames), name="auc", glmnet.fit=useAsGlmnetFit,
		lambda.min=lambda, lambda.1se=lambda)
	class(logregres)<-c("cv.MI.logreg", "cv.glmnet", "cv.lognet")
	return(logregres)
}