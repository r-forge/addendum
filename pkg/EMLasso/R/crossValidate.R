#' Crossvalidate a model
#' 
#' Crossvalidate a model
#' 
#' @param model model fit
#' @param \dots for flexibility in 'derived' implementation (passed on to \code{\link{collectImputationModels}}
#' 	in \code{crossValidate.EMLassoGLoMo})
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @note aids to generalize crossvalidation
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords crossvalidate model
#' @export
crossValidate<-function(model, ..., verbosity=0) UseMethod("crossValidate")
#' @rdname crossValidate
#' 
#' @aliases crossValidate.EMLassoGLoMo cv.EMLassoGLoMo-class cv.EMLasso.lognet
#' @method crossValidate EMLassoGLoMo
#' @usage \method{crossValidate}{EMLassoGLoMo}(model, ds=model$result[[1]]$ds, out=model$result[[1]]$out, wts=rep(1, nrow(ds)), imputeDs2FitDsProperties=model$imputeDs2FitDsProperties, imputations=10, ..., type.measure="auc", keepResultPerLambda=FALSE, verbosity=0)
#' @param ds dataset with predictors
#' @param out vector (binary factor) of outcomes
#' @param wts vector of weights (defaults to equal weights for all rows)
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} object that will provide the conversion from imputed
#' 	dataset to one that is ready for fitting the predictor model
#' @param imputations Number of multiple imputations on the complete dataset (defaults to 10)
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param keepResultPerLambda if \code{TRUE} (not the default), the individual results
#' 	from the \code{crossValidate.EMLasso1l} are also returned in an extra item
#' 	\code{resultPerLambda}
#' @return object that has as class: "cv." pasted before the class of \code{model}. Normally, \code{model} will
#' 	will be the return value of \code{\link{EMLasso}}, so this result is mainly the same as a \code{\link{cv.glmnet}}.
#' The added/altered items are:
#' \item{glmnet.fit}{is now the \code{model} passed in, so has more classes besides "glmnet" (e.g. "EMLasso")} 
#' \item{resultPerLambda }{matrix with one column per imputation. The top rows are the estimates for the criterion per
#' 	lambda, below that are their SD estimates. Not present if \code{keepResultPerLambda=FALSE}}
#' @seealso \code{\link{EMLasso}}, \code{\link{cv.glmnet}}
#' @keywords GLoMo EMLasso
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' require(addendum)
#' require(NumDfr)
#' require(GLoMo)
#' require(snowfall)
#' require(EMLasso)
#' sfInit(parallel = FALSE, cpus = 1)
#' sfLibrary(addendum)
#' sfLibrary(NumDfr)
#' sfLibrary(GLoMo)
#' sfLibrary(EMLasso)
#' iris.cpy<-randomNA(iris, n=0.1)
#' iris.emlognet<-EMLasso(ds=numdfr(iris.cpy), out=y,  
#' 	lambdas=c(0.03,0.002,0.0003), nrOfSamplesPerMDRow=7, verbosity=2,
#' 	convergenceChecker=convergenceCheckCreator(minIt=5, maxIt=10))
#' sfStop()
#' iris.cv.emlognet<-crossValidate(iris.emlognet, verbosity=2)
#' @export
crossValidate.EMLassoGLoMo<-function(model, ds=model$result[[1]]$ds, out=model$result[[1]]$out, 
	wts=rep(1, nrow(ds)), imputeDs2FitDsProperties=model$imputeDs2FitDsProperties, imputations=10, 
	..., type.measure="auc", keepResultPerLambda=FALSE, verbosity=0)
{
	lambda<-model$lambda
	impData<-collectImputationModels(model=model, ds=ds, ..., verbosity=verbosity-1)
	partres<-vapply(seq(imputations), function(i){
		catwif(verbosity > 1, "Imputation", i, "/", imputations)
		predict(impData, newdata=ds, out=out, wts=wts, type.measure=type.measure, verbosity=verbosity-1)
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
#' @rdname crossValidate
#' 
#' @aliases repeatedlyPredictOut
#' @param glomo GLoMo model to predict from
#' @param varsets list of character vectors holding the variables (names) to be checked
#' @param reps number of predictions
#' @param nfolds number of folds for crossvalidation
#' @param dsconvprops see \code{dsconvprops} (need to work on universal and correct naming...)
#' @param returnGroups if \code{TRUE}, a list is returned with the normal result as its \code{result}
#' 	item and a matrix holding the group assignment per repetition as the \code{groups} item.
#' @param returnCoefs if \code{TRUE}, a list is returned with the normal result as its \code{result}
#' 	item and a list of matrices holding the coefficient values per repetition and per fold as the 
#' 	\code{coefs} item.
#' @param reusabledata optional premade result of \code{\link{reusableDataForGLoMoSampling}}
#' @return List of the same length as \code{varsets} (unless it was length 1, then the first 
#' 	object is simply returned). Each item is a matrix with one row for each row in \code{ds}
#' 	and one column per \code{reps}, and holds the predicted probability in a crossvalidation.
#' @keywords GLoMo EMLasso crossvalidate
#' @export
repeatedlyPredictOut<-function(glomo, ds, out, varsets, reps=10, nfolds=10, dsconvprops=NULL, 
	returnGroups=FALSE, returnCoefs=FALSE, ..., reusabledata, verbosity=0)
{
	#IMPORTANT! Right now, this function is not adapted to the imputedDs style instead of dsconvprops
	#It has to be adapted, because currently the call to fit.logreg will (probably) fail or have
	#unexpected results
	warning("repeatedlyPredictOut needs adjustment to imputedDs. Better not to use it for now")
	if((missing(dsconvprops)) || (is.null(dsconvprops)))
	{
		catwif(verbosity > 0, "dsconvprops was not passed along, so needed to recalculate it")
		dsconvprops<-dfrConversionProps(dfr=ds, betweenColAndLevel="")
	}
	
	if(missing(reusabledata))
	{
		catwif(verbosity > 0, "Create reusable data for GLoMo sampling up front...")
	  reusabledata <- reusableDataForGLoMoSampling(glomo = glomo, dfr = ds, verbosity = verbosity - 10)
	}
	
	result<-replicate(length(varsets), matrix(NA, nrow=nrow(ds), ncol=reps), simplify=FALSE)
	if(returnGroups) allgrps<-matrix(NA, nrow=nrow(ds), ncol=reps)
	if(returnCoefs) 	coefs<-lapply(varsets, function(curvs){matrix(NA, nrow=length(curvs)+1, ncol=reps*nfolds)})
		
	
	for(currep in seq(reps))
	{
		catwif(verbosity > 1, "***repeat", currep, "/", reps)
		grps<-similarSizeGroups(ngroups=nfolds, nobs=nrow(ds), rand=TRUE)
		if(returnGroups) allgrps[,currep]<-grps
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
				try({#sometimes this goes wrong??
					curfit<-fit.logreg(ds=fitds, out=fitout, verbosity=verbosity-5, useCols=curUseVars, imputeDs2FitDsProperties=dsconvprops, ...)
					if(returnCoefs)
					{
						curcoef<-coef(curfit)
						rownames(coefs[[vsi]])<-rownames(curcoef)
						curcoef<-as.vector(curcoef)
						coefs[[vsi]][,((currep-1)*nfolds)+curfld]<-curcoef
					}
					useCols<-rownames(curfit$beta)
					#catwif(verbosity > 3, "Effectively used columns:", useCols)
					valdscurvarset<-valds[,useCols]
					curpreds<-predict(curfit, newx=valdscurvarset, type="response") #predicted probability
					result[[vsi]][valrows,currep]<-curpreds
				})
			}
		}
	}
	if(length(result)==1)
	{
		result<-result[[1]]
		if(returnCoefs) coefs<-coefs[[1]]
	}
	if(returnGroups | returnCoefs)
	{
		rv<-list(result=result)
		if(returnGroups) rv$groups<-allgrps
		if(returnCoefs) rv$coefs<-coefs
		return(rv)
	}
	else
	{
		return(result)
	}
}

#' @rdname crossValidate
#' 
#' @aliases repeatedPredictedProbAUC
#' @param reppredprob one of the matrices as return by \code{repeatedlyPredictOut}
#' @param groups vector/matrix of fold membership assignment. If nor present, 10 
#' 	random groups are created
#' @param onlyresult if \code{FALSE}, the return value holds (as list members) the
#' 	the overall AUC and sd, but also the results per repetition
#' @param glmnetlike if \code{TRUE}, \code{\link{calcAUC.glmnet}} is used to calculate AUC and sd,
#' 	otherwise \code{\link{calcAUC.Binary}} is used
#' @return named vector of length 2, holding the "AUC" and the "AUCSD"
#' @keywords GLoMo EMLasso crossvalidate
#' @export
repeatedPredictedProbAUC<-function(reppredprob, out, verbosity=0, groups, onlyresult=TRUE, glmnetlike=TRUE)
{
	if(is.factor(out))
	{
		out<-as.integer(out)
	}
	if(! is.logical(out))
	{
		out<- out > 1
	}
	if(missing(groups))
	{
		groups<-similarSizeGroups(ngroups=10, nobs=nrow(reppredprob), rand=TRUE)
	}
	#note: this is in fact multiple imputation!
	#As such, we can use the formulas on p86 of "Statistical Analysis with Missing Data" here
	if(glmnetlike)
	{
		repres<-calcAUC.glmnet(predmat=reppredprob, groups=groups, y=out, verbosity=verbosity-1)
		AUCs<-repres["cvm",]
		AUCVars<-(repres["cvsd",])^2
	}
	else
	{
		repres<-apply(reppredprob, 2, calcAUC.Binary, trueOnes=out, includeSE=TRUE)
		AUCs<-sapply(repres, "[[", "AUC")
		AUCVars<-sqrt(sapply(repres, "[[", "varAUC"))
	}
	theAUC<-mean(AUCs, na.rm = TRUE)
	D<-length(AUCs)
	avgWithinVar<-mean(AUCVars, na.rm = TRUE)
	betwImpVar<-var(AUCs)
	totalVar<-avgWithinVar + (D+1)/D*betwImpVar
	if(onlyresult)
	{
		return(c(AUC=theAUC, AUCSD=sqrt(totalVar)))
	}
	else
	{
		list(AUC=theAUC, AUCSD=sqrt(totalVar), allAUC=AUCs, allVars=AUCVars)
	}
}

#' @rdname crossValidate
#' 
#' @aliases cv.MI.logreg cv.MI.logreg-class
#' @param useVarNames names of columns to include in the model (character vector)
#' @param lambda value to use as lambda in the return value (note: ignored for the rest)
#' @param useAsGlmnetFit object that can be used for the \code{glmnet.fit} item in the return value
#' @return object of classes "cv.MI.logreg", "cv.glmnet" and "cv.lognet". Has exactly the items of
#' 	a \code{\link{cv.glmnet}} object
#' @keywords GLoMo EMLasso crossvalidate
#' @export
cv.MI.logreg<-function(glomo, ds, out, useVarNames, reps, dsconvprops, lambda, useAsGlmnetFit, ..., verbosity=0)
{
	preds<-repeatedlyPredictOut(glomo=glomo, ds=ds, out=out, varsets=list(useVarNames), reps=reps, 
		..., dsconvprops=dsconvprops, returnGroups=TRUE, verbosity=verbosity-1)
	groups<-preds$groups
	preds<-preds$result
	
	auc<-repeatedPredictedProbAUC(preds, out=out, verbosity=verbosity-1, groups=groups, onlyresult=TRUE, glmnetlike=TRUE)
	aucsd<-auc["AUCSD"]
	auc<-auc["AUC"]
	logregres<-list(lambda=lambda, cvm=auc, cvsd=aucsd, cvup=auc+aucsd,
		cvlo=auc-aucsd, nzero=length(useVarNames), name="auc", glmnet.fit=useAsGlmnetFit,
		lambda.min=lambda, lambda.1se=lambda)
	class(logregres)<-c("cv.MI.logreg", "cv.glmnet", "cv.lognet")
	return(logregres)
}