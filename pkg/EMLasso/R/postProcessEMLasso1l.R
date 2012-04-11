#' Postprocess EMLasso for 1 lambda result (e.g. add extra members or change class)
#' 
#' Postprocess EMLasso for 1 lambda result (e.g. add extra members or change class)
#' 
#' @aliases postProcessEMLasso1l
#' @param outcomeModel the final outcome model will be passed in here (e.g. \code{\link{glmnet}} object)
#' @param retval the return value of \code{\link{EMLasso.1l}} to be postprocessed
#' @param lastData dataset (completed) from the last EM iteration
#' @param imputeDs2FitDsProperties see \code{\link{imputeDs2FitDs}}
#' @param reusableForSampling Which of the rows of dfr/resp can be used for fitting the LASSO
#' @param reusableForvalidation minimum number of iterations before convergence is possible
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return Typically the passed along \code{retval} object with optionally some more items/class/attributes
#' @author Nick Sabbe \email{nick.sabbe@@ugent.be}
#' @seealso \code{\link{EMLasso.1l}}
#' @keywords EMLasso postprocess
#' @export
postProcessEMLasso1l<-function(outcomeModel, retval, lastData, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0) UseMethod("postProcessEMLasso1l")
#' @rdname postProcessEMLasso1l
#' 
#' @aliases postProcessEMLasso1l.default
#' @method postProcessEMLasso1l.default
#' @usage \method{postProcessEMLasso1l}{default}(outcomeModel, retval, lastData, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0)
#' @S3method postProcessEMLasso1l default
postProcessEMLasso1l.default<-function(outcomeModel, retval, lastData, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0)
{
	return(retval)
}
#' @rdname postProcessEMLasso1l
#' 
#' @aliases postProcessEMLasso1l.lognet
#' @method postProcessEMLasso1l.lognet
#' @usage \method{postProcessEMLasso1l}{lognet}(outcomeModel, retval, lastData, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0)
#' @S3method postProcessEMLasso1l lognet
postProcessEMLasso1l.lognet<-function(outcomeModel, retval, lastData, imputeDs2FitDsProperties, reusableForSampling, reusableForvalidation, verbosity=0)
{
	usedCols<-which(coef(retval$lasso.fit)[-1] != 0) #also holds the intercept!!!
	logreg.fit<-fit.logreg(lastData$ds[lastData$useForFit,], out=lastData$useOut,
		wts=lastData$weights[lastData$useForFit], verbosity=verbosity, useCols=usedCols,
		imputeDs2FitDsProperties=imputeDs2FitDsProperties)
	retval$logreg.fit=logreg.fit
	class(retval)<-c("EMLassoLog1l", class(retval))
	return(retval)
}
