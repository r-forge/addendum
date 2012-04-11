#' Tailormade weighted fitting of logistic glmnet
#' 
#' Do dummy coding on dataset and fit logistic (crossvalidated) glmnet
#' 
#' @param ds dataset (\code{\link{numdfr}} or \code{\link{data.frame}})
#' @param out outcome vector
#' @param lambda (single) lambda to use
#' @param weights weight vector per observation (does not have to sum to 1, and defaults to equal weights)
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param standardize if \code{TRUE} (NOT the default), all variables are standardized before fitting.
#' @param type.measure one of the crossvalidating measures provided by \code{\link{glmnet}} or
#' \code{NULL} (default) to not do crossvalidation
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}
#' @param \dots passed on to \code{\link{glmnet}} or \code{\link{cv.glmnet}}.
#' @return depending on \code{type.measure} being \code{NULL}, a \code{\link{glmnet}} or
#' \code{\link{cv.glmnet}} object.
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{glmnet}}
#' @keywords logistic LASSO glmnet
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' lnet<-fit.lognet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1)
#' cv.lnet<-fit.lognet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1, type.measure="auc")
#' @export
fit.lognet<-function(ds, out, lambda, weights=(rep(1, dim(ds)[1])),
	verbosity=0, standardize=FALSE, type.measure=NULL, 
	imputeDs2FitDsProperties=normalImputationConversion(), ...)
{
# 	if(missing(dfrConvData))
# 	{
# 		catwif(verbosity > 0, "dfrConvData was missing??")
# 		dfrConvData<-normalImputationConversion()
# 	}
	fit<-fit.glmnet(ds=ds, out=out, lambda=lambda, weights=weights,
		verbosity=verbosity, standardize=standardize, type.measure=type.measure, 
		imputeDs2FitDsProperties=imputeDs2FitDsProperties, family="binomial", ...)	
# 	catwif(verbosity > 0, "Create Design matrix")
# 	catwif(verbosity > 0, "Original dimension:", dim(dfr))
# 	if(missing(dfrConvData))
# 	{
# 		catwif(verbosity > 0, "dfrConvData was missing??")
# 		dfrConvData<-dfrConversionProps(dfr, "")
# 	}
# 	catwif(verbosity > 0, "Dimension of passed in dfr:", dim(dfr))
# 	dfr.mat<-factorsToDummyVariables(dfr, betweenColAndLevel="", dfrConvData=dfrConvData, verbosity=verbosity-1)
# 	catwif(verbosity > 0, "Resulting dimension:", dim(dfr.mat))
# 	catwif(verbosity > 0, "Actually fit glmnet")
# 	if(!is.null(type.measure))
# 	{
# 		if(length(lambda)==1) lambda<-c(lambda, lambda+0.000001)
# 		fit<-cv.glmnet(dfr.mat, resp, family="binomial", weights=weights, lambda=lambda,
# 			standardize=standardize, type.measure=type.measure, ...)
# 	}
# 	else
# 	{
# 		fit<-glmnet(dfr.mat, resp, family="binomial", weights=weights, lambda=lambda,
# 			standardize=standardize, ...)
# 	}
	return(fit)
}
