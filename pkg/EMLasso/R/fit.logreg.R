#' Fit logistic regression by using logistic LASSO with lambda=0
#' 
#' Fit logistic regression by using logistic LASSO with lambda=0
#' 
#' @param dfr dataset (\code{\link{numdfr}} or \code{\link{data.frame}})
#' @param resp outcome vector
#' @param wts weight vector per observation (does not have to sum to 1, and defaults to equal weights)
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param useCols restrict the regression to only include these columns as predictors
#' @param dfrConvData premade return value of \code{\link{dfrConversionProbs}} for that \code{glmnet}
#' and dataset
#' @param \dots passed on to \code{\link{glmnet}}/\code{\link{glm.fit}}. Not allowed: "x", "y", "family", ("weights", "lambda"), "standardize"
#' @return \code{\link{glmnet}} object
#' @note The warning in the old function pointed me to the fact that the weights are _not_
#' probability weights in a binomial glm!!
#' So, as an alternative, we use glmnet with lambda=0 !!!
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords fit logistic regression
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' lreg<-fit.logreg(iris, y, wts=runif(nrow(iris)), verbosity=1)
#' @export
fit.logreg<-function(dfr, resp, wts=rep(1, nrow(dfr)), verbosity=0, useCols=NULL,
	dfrConvData, ...)
{
	catwif(verbosity > 0, "dim dfr:", dim(dfr))
	if(missing(dfrConvData))
	{
		dfrConvData<-dfrConversionProbs(dfr, "")
	}
	dfr.mat<-factorsToDummyVariables(dfr, betweenColAndLevel = "", dfrConvData=dfrConvData, verbosity=verbosity-1) #Some factors are only partially there!
	catwif(verbosity > 0, "after factorsToDummyVariables dim dfr.mat:", dim(dfr.mat))
	if(!is.null(useCols))
	{
    if (is.character(useCols)) {
        useCols <- useCols[useCols %in% colnames(dfr.mat)]
        catwif(verbosity > 0, "useCols:", useCols)
    }
    else {
        catwif(verbosity > 0, "useCols in positions:", useCols)
        catwif(verbosity > 0, "useCols:", colnames(dfr.mat)[useCols])
    }
		dfr.mat<-dfr.mat[,useCols, drop=FALSE]
	}
	catwif(verbosity > 0, "dim dfr.mat:", dim(dfr.mat), ", l(resp):", length(resp), ", l(wts):", length(wts), "\n")
	if(ncol(dfr.mat) == 0)
	{
		catwif(verbosity > 0, "No predictors, so returning NULL")
		return(NULL)
	}
	if(all(wts==wts[1]))
	{
		#all weights are equal, so use simple logistic regression
		fit<-try(logregLikeGlmnet(dfr.mat, resp, verbosity=verbosity-1, ...))
	}
	else
	{
		fit<-try(glmnet(dfr.mat, resp, family="binomial", weights=wts, lambda=0, standardize=FALSE, ...))
	}
	catwif(verbosity > 0, "glm fit succeeded.")
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
	logregfit<-glm.fit(x=x, y=y, family=binomial(), ..., intercept=TRUE)
	catwif(verbosity > 0, "Coefficients found:")
	printif(verbosity > 0, logregfit$coefficients)
	a0<-logregfit$coefficients[1] #intercept
	beta<-new("dgCMatrix", Dim = as.integer(c(nc, 1)), Dimnames = list(cn, "s0"), 
		x = logregfit$coefficients[-1], p = as.integer(c(0,0)), i = as.integer(seq(nc)-1))
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