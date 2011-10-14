#' Calculate criteria similarly as is done in glmnet, but for EMLasso
#' 
#' Calculate criteria similarly as is done in glmnet, but for EMLasso
#' 
#' @param cv.eml list of \code{cvpart.emlasso} objects
#' @param type.measure see \code{\link{cv.glmnet}}
#' @param grouped see \code{\link{cv.glmnet}}
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return see \code{\link{cv.glmnet}}
#' @note used internally in \code{\link{EMLasso.1l.lognet.cv}}
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l.lognet.cv}}
#' @keywords crossvalidate glmnet
#' @export
mimic_cv_lognet<-function(cv.eml, type.measure="auc", grouped=TRUE, verbosity=0)
{
	catwif(verbosity>0, "Get outlist")
	outlist<-lapply(cv.eml, function(curfolddata){curfolddata$fitinfo$lasso.fit})
	lambda<-(outlist[[1]])$lambda
	is.offset<-(outlist[[1]])$offset
	if(is.offset)
	{
		warning("Offset not supported yet... Disregarding it (may result in erroneous values).")
	}
	offset<-NULL
	catwif(verbosity>0, "Get x")
	x<-do.call(rbind, lapply(cv.eml, function(curfolddata){factorsToDummyVariables(curfolddata$valsample$predicted)}))
	catwif(verbosity>0, "Get y")
	y<-do.call(c, lapply(cv.eml, function(curfolddata){curfolddata$valsample$resp}))
	catwif(verbosity>0, "Get weights")
	weights<-do.call(c, lapply(cv.eml, function(curfolddata){rep(1/curfolddata$valsample$numRepPerRow,curfolddata$valsample$numRepPerRow)}))
	catwif(verbosity>0, "Get foldid")
	foldid<-do.call(c, lapply(seq_along(cv.eml), function(curfoldi){rep(curfoldi,nrow((cv.eml[[curfoldi]])$valsample$predicted))}))

	catwif(verbosity>0, "Call cv.lognet")
	cv.lognet(outlist, lambda, x, y, weights, offset, foldid, type.measure, grouped)
}