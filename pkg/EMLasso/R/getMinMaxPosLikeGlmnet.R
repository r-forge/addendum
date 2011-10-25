#' Find a region of interest in a set of lambdas given criteria and their SE
#' 
#' Find a region of interest in a set of lambdas given criteria and their SE
#' 
#' @param object (main) object to get the region information from (for \code{default}: vector of lambda values)
#' @param \dots for flexibility in 'derived' implementation
#' @return list of class "lambdaregion":
#' \item{pos.optimum }{position of the optimum criterion in the set of lambdas}
#' \item{pos.higherlambda }{position in the set of lambdas of the highest lambda with criterion within one se of the optimum}
#' \item{pos.lowerlambda1 }{position in the set of lambdas of the lambda below that of the optimum, that has the inverted optimum}
#' \item{pos.lowerlambda2 }{position in the set of lambdas of the highest lambda below that of the optimum that does not have its criterion within one se of the optimum}
#' \item{lambda }{vector of lambda values (as passed in)}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords cv.glmnet lambda
#' @export
getMinMaxPosLikeGlmnet<-function(object,...) UseMethod("getMinMaxPosLikeGlmnet")

#' @rdname getMinMaxPosLikeGlmnet
#' 
#' @param cvm criteria value (see \code{\link{cv.glmnet}})
#' @param cvsd criteria value standard errors (see \code{\link{cv.glmnet}})
#' @param type.measure see \code{\link{cv.glmnet}}. If this is "auc", the \code{cvm} are negated before continuing.
#' @method getMinMaxPosLikeGlmnet default
#' @usage \method{getMinMaxPosLikeGlmnet}{default}(object, cvm, cvsd, type.measure,...)
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @keywords cv.glmnet lambda
#' @examples 
#' y<-sample(0:1, nrow(iris), replace=TRUE)
#' cvl<-fit.lognet(dfr=iris, resp=y, lambda=NULL, verbosity=10, type.measure="auc")
#' getMinMaxPosLikeGlmnet(cvl$lambda, cvl$cvm, cvl$cvsd, type.measure="auc")
#' @export
getMinMaxPosLikeGlmnet.default<-function(object, cvm, cvsd, type.measure,...)
{
	lambda<-object
	if(type.measure == "auc")
	{
		cvm<- -cvm
	}
	cvmin<-min(cvm)
	idmin<-(cvm <= cvmin)
	lambda.min<-max(lambda[idmin])
	idmin<-match(lambda.min, lambda)
	
	semin<-(cvm + cvsd)[idmin]
	wheremin<-(cvm < semin)
	lambda.higher<-max(lambda[wheremin])
	idhigher<-match(lambda.higher, lambda)
	if(idmin == idhigher)
	{
		idhigher<-which.max(lambda)
	}
	
	#one candidate for interesting lower lambda: the maximum
	candidate.lower<-(lambda < lambda.min)
	posOfOtherExtreme<-which.max(cvm[candidate.lower])
	idlower<-match((lambda[candidate.lower])[posOfOtherExtreme], lambda)
	#because the other one could easily be the extreme, we do it in
	#another way as well: highest lambda smaller than the one for the
	#optimum, that is _not_ within one se of the optimum
	candidate.lower<-((cvm >= semin) & (candidate.lower))
	if(sum(candidate.lower) == 0)
	{
		#none found, so take the extreme lower value for lambda
		idlower2<-which.min(lambda)
	}
	else
	{
		#now take the biggest lambda
		lambda.lower<-max(lambda[candidate.lower])
		idlower2<-match(lambda.lower, lambda)
	}
	
	retval<-list(pos.optimum=idmin, pos.higherlambda=idhigher, pos.lowerlambda1=idlower, 
		pos.lowerlambda2=idlower2, lambda=lambda)
	class(retval)<-"lambdaregion"
	return(retval)
}

#' @rdname getMinMaxPosLikeGlmnet
#' 
#' @method getMinMaxPosLikeGlmnet cv.glmnet
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @examples
#' y<-sample(0:1, nrow(iris), replace=TRUE)
#' cvl<-fit.lognet(dfr=iris, resp=y, lambda=NULL, verbosity=10, type.measure="auc")
#' getMinMaxPosLikeGlmnet(cvl)
#' @export
getMinMaxPosLikeGlmnet.cv.glmnet<-function(object,...)
{
	getMinMaxPosLikeGlmnet(object$lambda, object$cvm, object$cvsd, type.measure=names(object$name))
}
