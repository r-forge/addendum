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
#' @param type.measure one of the crossvalidating measures provided by \code{\link{cv.glmnet}} or
#' \code{NULL} (default) to not do crossvalidation
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}} and \code{\link{EMLasso}}
#' @param family see \code{\link{glmnet}}. Defaults to binomial (logistic regression)
#' @param \dots passed on to \code{\link{glmnet}} or \code{\link{cv.glmnet}}.
#' @param reRestrictIfRelevant \code{\link{cv.glmnet}}does not support passng a single \code{lambda} value. 
#' 	If this was requested, a dummy extra \code{lambda} is added. When this value is \code{TRUE} (default),
#' 	the results are afterwards restricted to the original \code{lambda}.
#' @return depending on \code{type.measure} being \code{NULL}, a \code{\link{glmnet}} or
#' \code{\link{cv.glmnet}} object.
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{glmnet}}, \code{\link{imputeDs2FitDs}}, \code{\link{EMLasso}}
#' @keywords logistic LASSO glmnet
#' @examples y<-rbinom(nrow(iris), 1, 0.5)
#' lnet<-fit.glmnet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1)
#' cv.lnet<-fit.glmnet(iris, y, lambda=0.05, weights=runif(nrow(iris)), verbosity=1, type.measure="auc")
#' @export
fit.glmnet<-function(ds, out, lambda, weights=(rep(1, dim(ds)[1])),
	verbosity=0, standardize=FALSE, type.measure=NULL, 
	imputeDs2FitDsProperties=normalImputationConversion(), family="binomial", ...,
	reRestrictIfRelevant=TRUE)
{
	catwif(verbosity > 0, "Create Design matrix")
	catwif(verbosity > 0, "Original dimension:", dim(ds))
	imputeDs2FitDsProperties<-imputeDs2FitDsProps(object=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity-1)
	catwif(verbosity > 0, "Dimension of passed in ds:", dim(ds))
	dfr.mat<-imputeDs2FitDs(conversionData=imputeDs2FitDsProperties,ds=ds,verbosity=verbosity-1)
	catwif(verbosity > 0, "Resulting dimension:", dim(dfr.mat))
	catwif(verbosity > 0, "Actually fit glmnet")
	if(!is.null(type.measure))
	{
		catwif(verbosity > 1, "Crossvalidating fit")
		if(length(lambda)==1)
		{
			catwif(verbosity > 1, "lambda had length 1 (", lambda, "). Adding 1 to get")
			orglambda<-lambda
			lambda<-c(lambda, lambda+0.000001)
		}
		else
		{
			reRestrictIfRelevant<-FALSE
		}
		fit<-cv.glmnet(dfr.mat, out, family=family, weights=weights, lambda=lambda,
			standardize=standardize, type.measure=type.measure, ...)
		if(reRestrictIfRelevant)
		{
			catwif(verbosity > 1, "restoring resulting cv.glmnet to single lambda", orglambda)
			keepLam<-match(orglambda, fit$lambda)
			fit<-restrictForLambda(fit, keepLam)
		}
	}
	else
	{
		catwif(verbosity > 1, "Non-crossvalidating fit")
		fit<-glmnet(dfr.mat, out, family=family, weights=weights, lambda=lambda,
			standardize=standardize, ...)
	}
	return(fit)
}
