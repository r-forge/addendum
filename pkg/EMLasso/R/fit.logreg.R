#' Fit logistic regression by using logistic LASSO with lambda=0
#' 
#' Fit logistic regression by using logistic LASSO with lambda=0
#' 
#' @param ds dataset (\code{\link{numdfr}} or \code{\link{data.frame}})
#' @param out outcome vector
#' @param wts weight vector per observation (does not have to sum to 1, and defaults to equal weights)
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param useCols restrict the regression to only include these columns as predictors
#' @param fakeLam optionally pass in the lambdas here
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}
#' @param \dots passed on to \code{\link{glmnet}}/\code{\link{glm.fit}}. Not allowed: "x", "y", "family", ("weights", "lambda"), "standardize"
#' @param type.measure see \code{\link{cv.glmnet}} if \code{NULL} (the default) no crossvalidation occurs
#' 	similar to what happens in fit.glmnet
#' @param nfolds see \code{\link{cv.glmnet}}
#' @param foldid see \code{\link{cv.glmnet}}
#' @return \code{\link{glmnet}} object
#' @note The warning in the old function pointed me to the fact that the weights are _not_
#' probability weights in a binomial glm!!
#' So, as an alternative, we use glmnet with lambda=0 !!!
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords fit logistic regression
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' lreg<-fit.logreg(iris, y, wts=runif(nrow(iris)), verbosity=1)
#' @export
fit.logreg<-function(ds, out, wts=rep(1, nrow(ds)), verbosity=0, useCols=NULL, fakeLam,
	imputeDs2FitDsProperties=normalImputationConversion(), ..., type.measure=NULL, nfolds=10, foldid)
{
	thisCall<-match.call()
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(object=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity-1)
	catwif(verbosity > 0, "Dimension of passed in ds:", dim(ds))
	ds.mat<-imputeDs2FitDs(conversionData=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity-1)
	catwif(verbosity > 0, "Resulting dimension:", dim(ds.mat))
	
	if(!is.null(type.measure))
	{
		obj<-.only.cv.reg(ds=ds.mat, out=out, wts=wts, verbosity=verbosity-1, useCols=useCols, fakeLam=fakeLam,
													 imputeDs2FitDsProperties=NULL, ..., type.measure=type.measure, nfolds=nfolds, foldid=foldid)
		return(obj)
	}
	else
	{
		allVars<-colnames(ds.mat)
		if(!is.null(useCols))
		{
			if(is.list(useCols))
			{
				allModels<-lapply(useCols, function(curUseCols){
					fit.logreg(ds.mat, out, wts=rep(1, nrow(ds)), verbosity=verbosity-5, useCols=curUseCols,
										 imputeDs2FitDsProperties=NULL, ...)
				})
				#allLams<-
				rv<-combineGlmnets(allModels, allVarNames=allVars, useCall=thisCall, verbosity=verbosity-1)
				if(! missing(fakeLam))
				{
					rv$lambda<-fakeLam
				}
				return(rv)
			}
	    if (is.character(useCols)) {
	    		unfoundUseCols<-setdiff(useCols, colnames(ds.mat))
	    		if(length(unfoundUseCols)>0) catw("Requested to use unfound columns:", unfoundUseCols)
	        useCols <- useCols[useCols %in% colnames(ds.mat)]
	        catwif(verbosity > 0, "useCols:", useCols)
	    }
	    else {
	        catwif(verbosity > 0, "useCols in positions:", useCols)
	        catwif(verbosity > 0, "useCols:", colnames(ds.mat)[useCols])
	    }
			ds.mat<-ds.mat[,useCols, drop=FALSE]
		}
		catwif(verbosity > 0, "dim ds.mat:", dim(ds.mat), ", l(out):", length(out), ", l(wts):", length(wts), "\n")
		if(ncol(ds.mat) == 0)
		{
			family<-"binomial"
			if("family" %in% names(list(...))) family<-list(...)$family
			useLambda<-0
			if(! missing(fakeLam)) useLambda<-fakeLam
			catwif(verbosity > 0, "No predictors, using glmnetNoPredictors")
			return(glmnetNoPredictors(y=out, xvarnames=allVars, family=family, weights=wts, lambda=useLambda))
		}
		if(all(wts==wts[1]))
		{
			#all weights are equal, so use simple logistic regression
			catwif(verbosity > 0, "All weights are equal, so use simple logistic regression")
			fit<-try(logregLikeGlmnet(ds.mat, out, verbosity=verbosity-1, ...))
			if(! missing(fakeLam))
			{
				fit$lambda<-fakeLam
			}
		}
		else
		{
			catwif(verbosity > 0, "Not all weights are equal, so use glmnet with lambda=0")
			if("family" %in% names(list(...)))
			{
				fit<-try(glmnet(ds.mat, out, weights=wts, lambda=0, standardize=FALSE, ...))
			}
			else
			{
				fit<-try(glmnet(ds.mat, out, family="binomial", weights=wts, lambda=0, standardize=FALSE, ...))
			}
			if(! missing(fakeLam))
			{
				fit$lambda<-fakeLam
			}
		}
		catwif(verbosity > 0, "glm fit succeeded.")
	}
	return(fit)
}

#' @rdname fit.logreg
#' 
#' @param x model matrix (as supported by \code{glm})
#' @param y outcomes (as supported by \code{glm})
#' @param useLambda lambda item of the return value
#' @return similar to the return value of \code{\link{glmnet}}
#' @keywords logistic regression glmnet
#' @export
logregLikeGlmnet<-function(x, y, useLambda=Inf, verbosity=0, ...)
{
	catwif(verbosity>0, "faking glmnet result from simple logistic regression")
	nc<-ncol(x)
	cn<-colnames(x)
	thisCall<-match.call()
	x<-cbind(1, x)
	colnames(x)[1]<-"(Intercept)"
	
	glmfitargs<-list(...)
	glmfitargs$x<-x
	glmfitargs$y<-y
	glmfitargs$intercept<-TRUE
	glmfitargs$family<-binomial() #avoid trouble with a passed down family argument
	logregfit<-do.call(glm.fit, glmfitargs)
	
	cofs<-logregfit$coefficients
	catwif(verbosity > 0, "Coefficients found:")
	printif(verbosity > 0, cofs)
	a0<-cofs[1] #intercept
	beta<-new("dgCMatrix", Dim = as.integer(c(nc, 1)), Dimnames = list(cn, "s0"), 
		x = cofs[-1], p = as.integer(c(0,length(cofs)-1)), i = as.integer(seq(nc)-1))
	lambda<-useLambda
	dev.ratio<-(1-logregfit$deviance )/logregfit$null.deviance
	#glmnet:
	#The fraction of (null) deviance explained (for "elnet", this is the R-square). 
	#The deviance calculations incorporate weights if present in the model. The 
	#deviance is defined to be -2*(loglike_sat - loglike), where loglike_sat is the 
	#log-likelihood for the saturated model (a model with a free parameter per 
	#observation). Hence dev.fraction=1-dev/nulldev.
	#glm
	#up to a constant, minus twice the maximized log-likelihood. Where sensible, the 
	#constant is chosen so that a saturated model has deviance zero.
	nulldev<-logregfit$null.deviance 
	#glmnet:
	#Null deviance (per observation). This is defined to be -2*(loglike_sat -loglike(Null)); 
	#The NULL model refers to the intercept model, except for the Cox, where it is the 0 model.
	#glm
	#The deviance for the null model, comparable with deviance. The null model will 
	#include the offset, and an intercept if there is one in the model. Note that this 
	#will be incorrect if the link function depends on the data other than through the 
	#fitted mean: specify a zero offset to force a correct calculation.
	df<-ncol(x)
	dim<-c(ncol(x), 1)
	nobs<-length(y)
	npasses<-1
	offset<-FALSE
	jerr<-"dummy"
	rv<-list(call=thisCall, a0=a0, beta=beta, lambda=lambda, dev.ratio=dev.ratio,
		nulldev=nulldev, df=df, dim=dim, nobs=nobs, npasses=npasses, offset=offset,
		jerr=jerr)
	class(rv)<-c("lognet", "glmnet")
	return(rv)
}

.only.cv.reg<-function(ds, out, wts=rep(1, nrow(ds)), verbosity=0, useCols=NULL, fakeLam,
								 imputeDs2FitDsProperties=normalImputationConversion(), ..., type.measure=NULL, nfolds=10, foldid)
{
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(object=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity-1)
	ds.mat<-imputeDs2FitDs(conversionData=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity-1)
	
	N<-nrow(ds.mat)
	if (missing(wts)) 
		wts<-rep(1, N)
	else wts<-as.double(wts)
	y<-drop(out)
	logreg.fit<-fit.logreg(ds.mat, y, wts, verbosity=verbosity-1, useCols=useCols, fakeLam=fakeLam,
												 imputeDs2FitDsProperties=NULL, ..., type.measure=NULL)
	is.offset<-logreg.fit$offset
	lambda<-logreg.fit$lambda
	nz<-sapply(predict(logreg.fit, type = "nonzero"), length)
	
	if (missing(foldid)) 
		foldid<-sample(rep(seq(nfolds), length = N))
	else nfolds<-max(foldid)
	outlist<-lapply(seq(nfolds), function(curfoldi){
		fitX<-ds.mat[foldid!=curfoldi,,drop=FALSE]
		fitY<-y[foldid!=curfoldi]
		fitWts<-wts[foldid!=curfoldi]
		resfit<-fit.logreg(fitX, fitY, fitWts, verbosity=verbosity-2, useCols=useCols, fakeLam=fakeLam,
							 imputeDs2FitDsProperties=NULL, ..., type.measure=NULL)
		return(resfit)
	})
	
	
	fun = paste("cv", class(logreg.fit)[[1]], sep = ".")
	cvstuff = do.call(fun, list(outlist, lambda, ds.mat, y, wts, 
															NULL, foldid, type.measure, TRUE))
	cvm = cvstuff$cvm
	cvsd = cvstuff$cvsd
	cvname = cvstuff$name
	out = list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + 
		cvsd, cvlo = cvm - cvsd, nzero = nz, name = cvname, glmnet.fit = logreg.fit)
	lamin = if (type.measure == "auc") 
		getmin(lambda, -cvm, cvsd)
	else getmin(lambda, cvm, cvsd)
	obj = c(out, as.list(lamin))
	class(obj) = "cv.glmnet"
	return(obj)
}